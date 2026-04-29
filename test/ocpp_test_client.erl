-module(ocpp_test_client).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/1, stop/1]).
-export([
    do/2,
    do_calls/2,
    connect/2,
    disconnect/1, disconnect/2,
    recv/1,
    message_id/0,
    version/1,
    boot_to/2, boot_to/3,
    make_boot_notification_request/2,
    make_boot_notification_response/3
]).

-doc """
Start a client for the station with ID `StationID`.
""".
start_link(StationID) ->
    Pid = spawn_link(fun() -> conn_control(StationID) end),
    {ok, Pid}.

-doc """
Stop the client. `Pid` should be the Pid returned by `start_client/1`.`
""".
stop(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _} ->
            ok
    after 5000 ->
        exit(timeout)
    end.

do_connect(StationID, Versions) ->
    Self = self(),
    F = fun() ->
        case ocpp_station:connect(StationID, Versions) of
            {ok, Version} ->
                Self ! {connected, Version},
                conn(StationID, Version);
            Reason ->
                Self ! {not_connected, Reason}
        end
    end,
    {Pid, _} = spawn_monitor(F),
    receive
        {connected, Version} ->
            {ok, {Pid, Version}};
        {not_connected, Reason} ->
            receive
                {'DOWN', _Ref, process, Pid, normal} ->
                    {error, Reason}
            end;
        {'DOWN', _Ref, process, Pid, Info} ->
            error({client_failed, Info})
    end.

conn(StationID, Version) ->
    receive
        {do, From, Fun} ->
            try Fun() of
                Result ->
                    From ! {result, Result}
            catch
                Error:Reason ->
                    From ! {error, {Error, Reason}}
            end,
            conn(StationID, Version);
        stop ->
            %% TODO disconnect politely
            stopped
    end.

conn_control(StationID) ->
    receive
        {connect, From, Versions} ->
            case do_connect(StationID, Versions) of
                {ok, {Pid, Version}} ->
                    From ! {ok, {self(), StationID}},
                    conn_control(StationID, Pid, Version);
                {error, Error} ->
                    From ! Error,
                    conn_control(StationID)
            end;
        stop ->
            ok;
        {disconnect, From} ->
            From ! disconnected,
            conn_control(StationID);
        Other ->
            error({unexpected_message, Other})
    end.

conn_control(StationID, Pid, Version) ->
    receive
        {'DOWN', _, process, Pid, Info} when Info =/= normal ->
            error({client_failed, Info});
        {do, From, Fun} ->
            Pid ! {do, From, Fun},
            conn_control(StationID, Pid, Version);
        {disconnect, From} ->
            stop_conn(Pid),
            From ! disconnected,
            conn_control(StationID);
        stop ->
            %% stop the client, we wait for the 'DOWN' message to transition states
            stop_conn(Pid),
            self() ! stop,
            conn_control(StationID);
        {exit, From, Reason} ->
            exit(Pid, Reason),
            receive
                {'DOWN', _, process, Pid, _} ->
                    From ! disconnected
            end,
            conn_control(StationID);
        {version, From} ->
            From ! {version, Version},
            conn_control(StationID, Pid, Version);
        Message ->
            error({unexpected_message, Message})
    end.

stop_conn(Pid) ->
    Pid ! stop,
    receive
        {'DOWN', _, process, Pid, normal} ->
            ok
    end.

-doc """
Connect the OCPP client process to its station using the provided list
of `Versions`. Note that the client already knows the ID of the
station it should connect to. Returns a tuple `{Pid, StationID,
Version}` where `Version` is the OCPP version for the connection.
""".
connect({Pid, _}, Versions) ->
    connect(Pid, Versions);
connect(Pid, Versions) ->
    Pid ! {connect, self(), Versions},
    receive
        {ok, ConnHandle} ->
            ConnHandle;
        {error, _} = Error ->
            Error
    after 1000 ->
        error(timeout)
    end.

-doc """
Dicsonnect the client from the server without shutting it down.
""".
disconnect({Pid, _}) ->
    disconnect(Pid);
disconnect(Pid) ->
    Pid ! {disconnect, self()},
    receive
        disconnected -> ok
    end.

-doc """
Disconnect by exiting with `Reason`.
""".
disconnect({Pid, _}, Reason) ->
    disconnect(Pid, Reason);
disconnect(Pid, Reason) ->
    Pid ! {exit, self(), Reason},
    receive
        disconnected -> ok
    end.

-doc """
Sends `Fun` to the client to be executed. The return value of `Fun` is
returned.
""".
do({Pid, _}, Fun) ->
    Pid ! {do, self(), Fun},
    receive
        {result, Result} ->
            Result;
        {error, {Class, Reason}} ->
            %% Re-create the error in this process. No stack trace,
            %% but this lets us use things like ?assertError() on the
            %% code in Fun
            erlang:Class(Reason);
        Other ->
            error({unexpected_result, Other})
    end.

-doc """
Receive a message sent by the ocpp_station to the client connection
process.
""".
recv(Timeout) ->
    receive
        {ocpp, {rpcsend, Reply}} ->
            Reply;
        Message ->
            error({unexpected_message, Message})
    after Timeout ->
        error(timeout)
    end.

-doc """
Generate a unique message ID.
""".
message_id() ->
    integer_to_binary(erlang:unique_integer([positive]), 36).

-doc """
Make a series of RPC CALLs as the client. Calls are described as
tuplse `{Request, Response}` where `Request` and `Response` are OCPP
request and response messages respectively (see `m:ocpp_message`).
Each call includes with it a response to be sent back to the client by
the ocpp_station; however, `Response` may be set to `undefined` if no
response is to be sent.
""".
do_calls(Conn = {_, StationID}, Messages) ->
    Version = version(Conn),
    F = fun() ->
        lists:foreach(
            fun
                ({Request, undefined}) ->
                    Call = ocpp_rpc:call(Request, message_id()),
                    ok = ocpp_station:rpc(StationID, ocpp_rpc:encode(Call));
                ({Request, Response}) ->
                    Call = ocpp_rpc:call(Request, message_id()),
                    ok = ocpp_station:rpc(StationID, ocpp_rpc:encode(Call)),
                    ok = ocpp_station:reply(StationID, ocpp_rpc:id(Call), Response),
                    {ok, {callresult, _}} = ocpp_rpc:decode(Version, recv(100), [
                        {expected, ocpp_message:action(Request)}
                    ])
            end,
            Messages
        )
    end,
    do(Conn, F).

version({Pid, _}) ->
    version(Pid);
version(Pid) ->
    Pid ! {version, self()},
    receive
        {version, Version} ->
            Version
    end.

boot_to(Conn, State) ->
    boot_to(Conn, State, []).

boot_to(Conn, State, Options) ->
    Version = version(Conn),
    boot_to(Conn, State, Version, Options).

boot_to(Conn, accepted, Version, Options) ->
    boot(Conn, Version, 'Accepted', Options);
boot_to(Conn, rejected, Version, Options) ->
    boot(Conn, Version, 'Rejected', Options);
boot_to(Conn, pending, Version, Options) ->
    boot(Conn, Version, 'Pending', Options);
boot_to(Conn, provisioning, Version, Options) ->
    boot(Conn, Version, undefined, Options);
boot_to(Conn, offline, Version, Options) ->
    boot(Conn, Version, 'Accepted', Options),
    disconnect(Conn);
boot_to(Conn, reconnecting, Version, Options) ->
    boot_to(Conn, offline, Version, Options),
    connect(Conn, [Version]);
boot_to(_, connected, _, _) ->
    ok.

boot(Conn, Version, undefined, Options) ->
    BootNotificationRequest = make_boot_notification_request(Version, Options),
    do_calls(Conn, [{BootNotificationRequest, undefined}]);
boot(Conn, Version, Status, Options) ->
    BootNotificationRequest = make_boot_notification_request(Version, Options),
    BootNotificationResponse = make_boot_notification_response(Version, Status, Options),
    do_calls(Conn, [{BootNotificationRequest, BootNotificationResponse}]).

make_boot_notification_response(Version, Status, Options) ->
    CurrentTime = proplists:get_value(currentTime, Options, {{2025, 9, 5}, {17, 28, 12}}),
    Interval = proplists:get_value(interval, Options, 0),
    {ok, Response} = ocpp_message:new(
        Version,
        ~"BootNotificationResponse",
        #{status => Status, currentTime => CurrentTime, interval => Interval}
    ),
    Response.

make_boot_notification_request('1.6', Options) ->
    Model = proplists:get_value(chargePointModel, Options, <<"ocpp_test_client">>),
    Vendor = proplists:get_value(chargePointVendor, Options, <<"eunit">>),
    {ok, Msg} = ocpp_message:new('1.6', ~"BootNotificationRequest", #{
        chargePointModel => Model, chargePointVendor => Vendor
    }),
    Msg;
make_boot_notification_request(Version, Options) ->
    Reason = proplists:get_value(reason, Options, 'PowerUp'),
    ChargingStation = proplists:get_value(
        chargingStation,
        Options,
        #{model => <<"ocpp_test_client">>, vendorName => <<"eunit">>}
    ),
    {ok, Request} = ocpp_message:new(
        Version,
        ~"BootNotificationRequest",
        #{reason => Reason, chargingStation => ChargingStation}
    ),
    Request.
