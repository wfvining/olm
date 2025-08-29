-module(test_station).

-include_lib("eunit/include/eunit.hrl").

-define(withStation(StationID, Tests),
    {foreach, setup_station(StationID), fun teardown_station/1, Tests}
).
-define(withConnectedStation(StationID, InState, Tests),
    {foreach, fun() -> setup_connected_station(StationID, InState) end,
        fun teardown_connected_station/1, Tests}
).

setup_station(StationID) ->
    fun() -> setup_station(StationID, []) end.

setup_deps() ->
    application:ensure_all_started(gproc).

teardown_deps(_) ->
    application:stop(gproc).

setup_station(StationID, _Options) ->
    ok = meck:new(ocpp_station_manager),
    meck:expect(
        ocpp_station_manager,
        connect,
        fun(_, Versions) -> {ok, lists:max(Versions)} end
    ),
    {ok, _Pid} = ocpp_station:start_link(StationID),
    StationID.

teardown_station(StationID) ->
    catch ocpp_station:stop(StationID),
    meck:unload(ocpp_station_manager).

setup_connected_station(StationID, InState) ->
    StationID = setup_station(StationID, []),
    Self = self(),
    Pid = spawn(
        fun() ->
            {ok, '2.0.1'} = ocpp_station:connect(StationID, ['2.0.1']),
            Loop = fun Conn(State) ->
                receive
                    stop ->
                        ok;
                    {goto, NewState} ->
                        establish_state(StationID, State, NewState),
                        Self ! ready,
                        Conn(State)
                end
            end,
            Loop(connected)
        end
    ),
    Pid ! {goto, InState},
    receive
        ready -> ok
    end,
    {StationID, Pid}.

teardown_connected_station({StationID, ConnPid}) ->
    case is_process_alive(ConnPid) of
        true ->
            ConnPid ! stop;
        false ->
            ok
    end,
    teardown_station(StationID).

establish_state(_, connected, connected) ->
    ok;
establish_state(StationID, State, NewState) ->
    error(not_implemented).

connect_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?withStation(
            <<"foo">>,
            [
                fun connect_invalid_station/1,
                fun connect_station/1,
                fun connect_station_again/1,
                fun connect_station_twice/1
            ]
        )}.

down_test_() ->
    %% TODO these tests should succeed no matter what state the station is in.
    {setup, fun setup_deps/0, fun teardown_deps/1, [
        ?withConnectedStation(
            list_to_binary(atom_to_list(?FUNCTION_NAME) ++ atom_to_list(State)),
            State,
            [
                fun reconnect_voluntary/1,
                fun reconnect_killed/1
            ]
        )
     || State <- [
            connected
            %% TODO None of these are supported yet.
            %% provisioning,
            %% boot_pending,
            %% provisioning_call_pending,
            %% accepted,
            %% call_pending,
            %% reconnecting
        ]
        %% TODO what should happen to the connection process if the station process exits?
    ]}.

connect_rejected_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?withStation(
            <<"connection_rejected_test">>,
            [
                fun connection_rejected/1,
                fun connection_handler_error/1
            ]
        )}.

connection_handler_error(StationID) ->
    {"connection fails when handler returns {error, _}", fun() ->
        meck:expect(ocpp_station_manager, connect, fun
            (_, ['1.6']) -> {error, because};
            (_, ['2.0.1']) -> {ok, '2.0.1'}
        end),
        ?assertEqual({error, because}, ocpp_station:connect(StationID, ['1.6'])),
        ?assertEqual({ok, '2.0.1'}, ocpp_station:connect(StationID, ['2.0.1'])),
        ?assert(meck:validate(ocpp_station_manager))
    end}.

connection_rejected(StationID) ->
    {"connection fails when rejected by handler", fun() ->
        %% ocpp_station_manager mock is created in setup. We just
        %% need to replace the connect expectation so it closes
        %% the connection.
        meck:expect(ocpp_station_manager, connect, fun
            (_, ['2.1']) -> close;
            (_, ['1.6']) -> {ok, '1.6'}
        end),
        ?assertEqual(close, ocpp_station:connect(StationID, ['2.1'])),
        ?assertEqual({ok, '1.6'}, ocpp_station:connect(StationID, ['1.6'])),
        ?assertEqual({error, already_connected}, ocpp_station:connect(StationID, ['1.6'])),
        ?assert(meck:validate(ocpp_station_manager))
    end}.

connect_invalid_station(_StationID) ->
    {"connecting to a station ID that does not exist fails",
        ?_assertError(nostation, ocpp_station:connect(<<"invalid">>, ['2.0.1']))}.

connect_station(StationID) ->
    {"can connect to a station", fun() ->
        {ok, '2.0.1'} = ocpp_station:connect(StationID, ['2.0.1'])
    end}.

connect_station_again(StationID) ->
    {"the same process can't connect to a station twice", fun() ->
        {ok, '2.1'} = ocpp_station:connect(StationID, ['1.6', '2.0.1', '2.1']),
        ?assertMatch({error, already_connected}, ocpp_station:connect(StationID, ['2.1']))
    end}.

connect_station_twice(StationID) ->
    {"two processes can't connect to the same station", fun() ->
        Self = self(),
        F = fun() ->
            Self ! ocpp_station:connect(StationID, ['1.6']),
            receive
                stop -> ok
            after 5000 -> error(timeout)
            end
        end,
        Pid1 = spawn_link(F),
        Pid2 = spawn_link(F),
        Messages = [
            receive
                M1 -> M1
            end,
            receive
                M2 -> M2
            end
        ],
        Pid1 ! Pid2 ! stop,
        ?assert(lists:member({ok, '1.6'}, Messages)),
        ?assert(lists:member({error, already_connected}, Messages))
    end}.

reconnect_voluntary({StationID, Pid}) ->
    {"different process can connect to station after original connection ends voluntarily", fun() ->
            true = link(Pid),
            process_flag(trap_exit, true),
            Pid ! stop,
            receive
                {'EXIT', Pid, normal} ->
                    ok
            end,
            ?assertEqual({ok, '2.0.1'}, ocpp_station:connect(StationID, ['2.0.1']))
        end}.

reconnect_killed({StationID, Pid}) ->
    {"different process can connect to station after original connection exits abnormally", fun() ->
            true = link(Pid),
            process_flag(trap_exit, true),
            exit(Pid, kill),
            receive
                {'EXIT', Pid, killed} ->
                    ok
            end,
            ?assertEqual({ok, '2.0.1'}, ocpp_station:connect(StationID, ['2.0.1']))
        end}.

rpc_from_wrong_process_test_() ->
    %% TODO expand this to all other states
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?withConnectedStation(
            atom_to_binary(?FUNCTION_NAME),
            connected,
            [
                fun({StationID, _Pid}) ->
                    {"ocpp_station:rpc/2 fails from process other than connected process",
                        ?_assertEqual(
                            {error, not_connected}, ocpp_station:rpc(StationID, <<"[]">>)
                        )}
                end
            ]
        )}.

connect(StationID, Versions) ->
    Self = self(),
    F = fun() ->
        {ok, Version} = ocpp_station:connect(StationID, Versions),
        Self ! {connected, Version},
        conn(StationID, Version)
    end,
    {Pid, _} = spawn_monitor(F),
    receive
        {connected, Version} ->
            {Pid, Version};
        {'DOWN', _Ref, process, Pid, Info} ->
            error({client_failed, Info})
    end.

conn(StationID, Version) ->
    receive
        {do, From, Fun} ->
            Result = Fun(),
            From ! {result, Result},
            conn(StationID, Version);
        stop ->
            %% TODO disconnect politely
            ?debugMsg("conn exiting"),
            stopped
    end.

ocpp_client_recv(Timeout) ->
    receive
        {ocpp, {rpcsend, Reply}} ->
            Reply
    after Timeout ->
        error(timeout)
    end.

setup_and_connect(StationID) ->
    fun() ->
        StationID = setup_station(StationID, []),
        {Pid, Version} = connect(StationID, ['2.0.1']),
        {Pid, StationID, Version}
    end.

teardown_conn({Pid, StationID, _}) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    %% ensure the connection process shuts down first.
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    end,
    teardown_station(StationID).

-define(testStationID, atom_to_binary(?FUNCTION_NAME)).

-define(stationTestSequence(StationID, Tests),
    {setup, setup_and_connect(StationID), fun teardown_conn/1, fun({Pid, _StationID, _Version}) ->
        {inorder, [Test(Pid) || Test <- Tests]}
    end}
).

-define(_stationTest(Description, ClientBody, ResultPattern),
    ?_stationTest((Description), (ClientBody), (ResultPattern), no_body)
).
-define(_stationTest(Description, ClientBody, ResultPattern, TestBody), fun(ConnPid) ->
    {Description, fun() ->
        Ref = monitor(process, ConnPid),
        F = fun() -> (ClientBody) end,
        ConnPid ! {do, self(), F},
        receive
            %% XXX if ResultPattern is just a variable name ("Result")
            {result, ResultPattern} ->
                FTest = fun() -> TestBody end,
                FTest();
            {result, Result} ->
                %% this will fail and give a nice error message
                ?assertMatch(ResultPattern, Result);
            {'DOWN', Ref, process, ConnPid, Reason} ->
                error({client_failed, Reason})
        after 5000 ->
            error(timeout)
        end
    end}
end).

%% test behavior of ocpp_station with respect to station initiated
%% messages.
provision_station_client_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?stationTestSequence(
            ?testStationID,
            [
                ?_stationTest(
                    "message other than a BootNotificationRequest rejected before provisioning",
                    ocpp_station:rpc(
                        ?testStationID,
                        ~B<[2,"0","Authorize",{"idToken":{"idToken":"","type":"NoAuthorization"}}]>
                    ),
                    {error, not_provisioned}
                ),
                ?_stationTest(
                    "start provisioning by sending a BootNotificationRequest",
                    ocpp_station:rpc(
                        ?testStationID,
                        <<
                            ~S<[2,"1","BootNotification",>,
                            ~S<{"chargingStation":{"model":"provision_test","vendorName":"eunit"},>,
                            ~S<"reason":"PowerUp"}]>
                        >>
                    ),
                    ok
                ),
                ?_stationTest(
                    "additional messages before BootNotificationResponse are rejected",
                    ocpp_station:rpc(
                        ?testStationID,
                        <<
                            ~S<[2,"2","TransactionEvent",>,
                            ~S<{"eventType":"Started","timestamp":"2025-01-01T00:00:00Z",>,
                            ~S<"triggerReason":"Authorized","seqNo":1,"offline":true,>,
                            ~S<"transactionInfo":{"transactionId":"test"}}]>
                        >>
                    ),
                    %% B01.FR.10 states that a security error should
                    %% be sent when "the charging station has
                    %% received a BootNotificaionRequest in which the
                    %% status is not Accepted." That seems to imply
                    %% disallowed messages prior to sending a
                    %% BootNotificationResponse should simply be dropped.
                    {error, not_provisioned}
                ),
                ?_stationTest(
                    "respond to BootNotificationRequest with status='Pending'",
                    begin
                        ocpp_station:reply(
                            ?testStationID,
                            <<"1">>,
                            ocpp_message_2_0_1:boot_notification_response(
                                #{
                                    status => 'Pending',
                                    interval => 1,
                                    currentTime => {{2025, 1, 1}, {0, 0, 0}}
                                }
                            )
                        ),
                        ocpp_client_recv(1000)
                    end,
                    RPCReply,
                    begin
                        DecodedRPC = ocpp_rpc:decode(
                            '2.0.1',
                            RPCReply,
                            [{expected, <<"BootNotification">>}]
                        ),
                        ?assertMatch({ok, {callresult, _}}, DecodedRPC),
                        {ok, {callresult, RPCResponse}} = DecodedRPC,
                        ?assertEqual(<<"1">>, ocpp_rpc:id(RPCResponse)),
                        ExpectedMessage = ocpp_message_2_0_1:boot_notification_response(
                            #{
                                status => 'Pending',
                                interval => 1,
                                currentTime => {{2025, 1, 1}, {0, 0, 0}}
                            }
                        ),
                        ?assertEqual(ExpectedMessage, ocpp_rpc:payload(RPCResponse))
                    end
                )
            ]
        )}.

%% TODO Things to test:

%% 1. for all states after receiving a BootNotificationRequest that is
%%    not (yet) rejected: 
%%    - a new RPC CALL comes in with a DIFFERENT message ID - this
%%      indicates that the station timed out waiting for the reply and
%%      has either sent a new message or retried the timed out message.
%%      Expected behavior: processing of the old message is abandoned
%%      and the new message is processed.
%%    - a new RPC CALL comes in with the SAME message ID - this is an
%%      error and should not happen; however, we should plan to handle
%%      it. Expected behavior: drop the message.

%% 2. BootNotificationRequest while a previous BootNotificaionRequest 
%%    is being processed. The most likely cause of this would be that
%%    handler has failed and the station has timed out waiting for a
%%    response. Expectation should be to drop the old request and
%%    process the new one.

%% 3. Any unsolicited message while boot is pending, other than
%%    another BootNotificationRequest (after the designated interval,
%%    if specified in the previous BootNotificationResponse) SHALL
%%    result in a CALLERROR: SecurityError (Requirement B01.FR.10,
%%    B02.FR.09, & B03.FR.07)

%% 4. The CSMS (ocpp_station state machine) SHALL NOT send any
%%    messages unless it has first sent a BootNotificationResponse
%%    with status Rejected. (B03.FR.08)

%% 5. The CSMS SHALL NOT send any messages other than 
%%    - SetVariablesRequest
%%    - GetVariablesRequest
%%    - TriggerMessageRequest
%%    - GetReportRequest
%%    NOTE In 2.0.1 the network profile cannot be updated in Pending
%%    state; however, in 2.1 it is exposed by the device model and can
%%    be configured via a SetVariablesRequest (if supported by the
%%    charging station).

%% 6. BootNotificaionRequest arrives at some time after a
%%    BootNotificationResponse(status=Accepted) has been sent but
%%    before any other messages arrive from the station. This
%%    indicates that the response never arrived at the station. We
%%    should reset to a provisioning state and send a new response.

%% 7. BootNotificationRequest arrives at some time after the station
%%    was accepted and after other messages have been received. This
%%    is an error. In OCPP 2.0.1 and 2.1 this is covered by
%%    requirements B01.FR.05. In OCPP 1.6 the requirement is less
%%    clear, but on first boot there should be a status notification
%%    for each connector and on re-connection there should be a
%%    notification for components whose status has changed. Because no
%%    state may have changed and no status notifications may arrive
%%    the dividing line between a valid BootNotificationRequest retry
%%    and an invalid BootNotificationRequest must be the receipt of
%%    any other message.
