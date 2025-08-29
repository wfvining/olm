-module(ocpp_station).
-moduledoc """
The generic state machine representing the state of a **charging
station** (in OCPP 1.6 the **charge point** is equivalent to the
charging station modeled by this module).

Only the high level state of a station is modeled by this state
machine. This includes the conneciton state of the station, whether it
has been provisioned (i.e. a `BootNotificationRequest` has been
received and accepted), and whether there is an outstanding `RPCCALL`.
Additionally, station level variables are held by the `ocpp_station`
process.
""".

-behavior(gen_statem).

-export([start_link/1, start_link/2, stop/1]).
-export([connect/2, rpc/2, reply/3]).
-export([init/1, callback_mode/0]).
%% State callbacks
-export([
    unprovisioned/3,
    connected/3,
    provisioning/3,
    boot_pending/3,
    provisioning_call_pending/3,
    accepted/3,
    call_pending/3,
    offline/3,
    reconnecting/3
]).

-define(registry(StationID), {via, gproc, ?name(StationID)}).
-define(name(Name), {n, l, {ocpp_station, Name, station}}).

-record(state, {
    stationid :: binary(),
    connection ::
        {pending, gen_statem:from(), pid()} | {ocpp:version(), pid(), reference()} | undefined,
    pending_call :: {ocpp_rpc:messageid(), binary()} | undefined,
    rpc_from :: undefined | gen_statem:from(),
    rpc_call :: undefined | {ocpp_handler:rpc_handle(), ocpp_rpc:call()}
}).

start_link(StationID, Options) ->
    gen_statem:start_link(?registry(StationID), ?MODULE, {StationID, Options}, []).

start_link(StationID) ->
    start_link(StationID, []).

-doc """
Connect the calling process to the station. 

There are three possible returns.

* `{ok, OCPPVersion}` indicates that the connection has been accepted
  using the OCPP version identified by `OCPPVersion`.
* `{error, Reason}` can indicate either that another process has
  already connected to the station (`Reason = already_connected`) or
  that an error occured processing the connection request. The HTTP
  server would most likely respond to the websocket upgrade request
  with a 500 HTTP status.
* `close` indicates that the websocket handshake should be completed
  and immediately closed. This is the return value if `Versions` does
  not include a version supported by the handler.

If no station exists with the given `StationID` the call fails with
reason `noproc`
""".
-spec connect(binary(), Versions :: [ocpp:version()]) ->
    {ok, ocpp:version()} | {error, term()} | close.
connect(StationID, Versions) ->
    try
        gen_statem:call(?registry(StationID), {connect, self(), Versions})
    catch
        exit:{noproc, _} ->
            error(nostation)
    end.

-doc """
Stop the station.
""".
-spec stop(binary()) -> ok.
stop(StationID) ->
    gen_statem:stop(?registry(StationID)).

-doc """
Send an undecoded RPC request to the station for processing.
""".
rpc(StationID, RPCBinary) ->
    gen_statem:call(?registry(StationID), {rpc, self(), RPCBinary}).

-doc """
Send a CALLRESULT RPC message to the station.

If the message cannot be sent and `{error, Reason}` tuple is returned.
This will happen if the station is not in a state that permits the
message to be sent. For example, if the station has disconnected or if
the response is no longer relevant due to a subsequent CALL message
from the station.
""".
-spec reply(binary(), binary(), ocpp_message:message()) ->
    ok | {error, disconnected | call_dropped}.
reply(StationID, MessageID, Payload) ->
    RPC = ocpp_rpc:callresult(Payload, MessageID),
    gen_statem:call(?registry(StationID), {send, {callresult, RPC}}).

callback_mode() ->
    state_functions.

init({StationID, _Options}) ->
    process_flag(trap_exit, true),
    {ok, unprovisioned, #state{stationid = StationID}}.

unprovisioned({call, From}, {connect, Pid, Versions}, State) ->
    case ocpp_station_manager:connect(State#state.stationid, Versions) of
        {ok, Version} ->
            logger:info(
                "Connection to station ~p accepted at version ~p (ConnectionPid = ~p)",
                [State#state.stationid, Version, Pid]
            ),
            Ref = erlang:monitor(process, Pid),
            {next_state, connected, State#state{connection = {Version, Pid, Ref}}, [
                {reply, From, {ok, Version}}
            ]};
        close ->
            logger:info(
                "Connection to station ~p rejected (ConnectionPid = ~p)",
                [State#state.stationid, Pid]
            ),
            {keep_state_and_data, [{reply, From, close}]};
        {error, Reason} ->
            logger:info(
                "Connection to station ~p failed: ~p (ConnectionPid = ~p)",
                [State#state.stationid, Reason, Pid]
            ),
            {keep_state_and_data, [{reply, From, {error, Reason}}]}
    end;
unprovisioned({call, From}, {rpc, _, _}, _State) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]}.

connected(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info(
        "Connection down before provisioning. ConnectionPid=~p, Reason=~p",
        [Pid, Reason]
    ),
    {next_state, unprovisioned, State#state{connection = undefined}};
connected({call, From}, {connect, _, _}, _State) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]};
connected(
    {call, From},
    {rpc, Pid, RPCBinary},
    State
) ->
    handle_rpc(RPCBinary, Pid, From, State);
connected(internal, {call, RPCCall}, #state{rpc_call = undefined} = State) ->
    Payload = ocpp_rpc:payload(RPCCall),
    case ocpp_message:type(Payload) of
        'BootNotificationRequest' ->
            ocpp_handler:rpc_call(
                State#state.stationid, ocpp_message:type(Payload), ocpp_rpc:id(RPCCall), Payload
            ),
            {next_state, provisioning, State#state{rpc_from = undefined, rpc_call = RPCCall}, [
                {reply, State#state.rpc_from, ok}
            ]};
        MessageType ->
            logger:warning(
                "~p got unexpected ~p message before provisioning (MessageID = ~p)",
                [State#state.stationid, MessageType, ocpp_rpc:id(RPCCall)]
            ),
            {keep_state, State#state{rpc_from = undefined}, [
                {reply, State#state.rpc_from, {error, not_provisioned}}
            ]}
    end.

provisioning({call, From}, {rpc, Pid, RPCBinary}, State) ->
    handle_rpc(RPCBinary, Pid, From, State);
provisioning(internal, {call, RPCCall}, #state{rpc_call = PendingCall} = State) ->
    Payload = ocpp_rpc:payload(RPCCall),
    case ocpp_message:type(Payload) of
        'BootNotificationRequest' ->
            logger:warning(
                "Received new BootNotification~nID: ~p~n~p~n "
                "while processing a BootNotificaion~nID: ~p~n~p~n"
                "pending request dropped",
                [
                    ocpp_rpc:id(RPCCall),
                    Payload,
                    ocpp_rpc:id(PendingCall),
                    ocpp_rpc:payload(PendingCall)
                ]
            ),
            ocpp_handler:rpc_call(
                State#state.stationid, ocpp_rpc:id(RPCCall), ocpp_message:type(Payload), Payload
            ),
            {next_state, provisioning, State#state{rpc_from = undefined, rpc_call = RPCCall}, [
                {reply, State#state.rpc_from, ok}
            ]};
        MessageType ->
            logger:warning(
                "~p got unexpected ~p message before provisioning (MessageID = ~p)",
                [State#state.stationid, MessageType, ocpp_rpc:id(RPCCall)]
            ),
            {keep_state, State#state{rpc_from = undefined}, [
                {reply, State#state.rpc_from, {error, not_provisioned}}
            ]}
    end;
provisioning({call, From}, {send, {callresult, RPC}}, #state{rpc_call = PendingCall} = State) ->
    case {ocpp_rpc:id(RPC), ocpp_rpc:id(PendingCall)} of
        {SendID, PendingID} when SendID =:= PendingID ->
            send_response(State#state.connection, RPC),
            Payload = ocpp_rpc:payload(RPC),
            try ocpp_message:get(status, Payload) of
                'Accepted' ->
                    {next_state, accepted, State#state{rpc_call = undefined}, [{reply, From, ok}]};
                'Rejected' ->
                    {next_state, connected, State#state{rpc_call = undefined}, [{reply, From, ok}]};
                'Pending' ->
                    {next_state, boot_pending, State#state{rpc_call = undefined}, [
                        {reply, From, ok}
                    ]}
            catch
                error:{badkey, _} ->
                    logger:warning(
                        "response is not a valid BootNotificationResponse~nID:~p~n~p",
                        [SendID, Payload]
                    ),
                    {keep_state_and_data, [{reply, From, {error, bad_message}}]}
            end;
        {SendID, PendingID} when SendID =/= PendingID ->
            logger:warning(
                "Got response for message ~p, but ~p is pending~nresponse dropped.",
                [SendID, PendingID]
            ),
            {keep_state_and_data, [{reply, From, {error, call_dropped}}]}
    end;
provisioning(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Connection process ~p down before BootNotificationResponse sent~n~p", [Pid, Reason]),
    {next_state, unprovisioned, State#state{connection = undefined, rpc_call = undefined}}.

boot_pending(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Connection process ~p down while boot Pending~n~p", [Pid, Reason]),
    {next_state, unprovisioned, State#state{connection = undefined, rpc_call = undefined}};
boot_pending(_, _, _) ->
    todo.

provisioning_call_pending(_, _, _) ->
    todo.

accepted(_, _, _) ->
    todo.

call_pending(_, _, _) ->
    todo.

offline(_, _, _) ->
    todo.

reconnecting(_, _, _) ->
    todo.

handle_rpc(
    RPCBinary, Pid, From, #state{connection = {Version, Pid, _}, pending_call = PendingCall} = State
) ->
    Opts =
        case PendingCall of
            undefined -> [];
            {MessageID, Action} -> [{expected, {MessageID, Action}}]
        end,
    case ocpp_rpc:decode(Version, RPCBinary, Opts) of
        {ok, RPC} ->
            {keep_state, State#state{rpc_from = From}, [{next_event, internal, RPC}]};
        {error, Reason} ->
            logger:warning("RPC decode failed: ~p. Message = ~p", [Reason, RPCBinary]),
            {keep_state_and_data, [{reply, From, Reason}]}
    end;
handle_rpc(_, Pid, From, #state{connection = {_, ConnPid, _}}) when Pid =/= ConnPid ->
    logger:warning(
        "Got unexpected RPC from ~p which is not the connected process (~p). Message dropped.",
        [Pid, ConnPid]
    ),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
handle_rpc(_, Pid, From, #state{connection = undefined}) ->
    logger:warning(
        "Got unexpected RPC from ~p but station is not connected. Message dropped.",
        [Pid]
    ),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]}.

send_response({_, Conn, _}, RPC) ->
    Conn ! {ocpp, {rpcsend, ocpp_rpc:encode(RPC)}}.
