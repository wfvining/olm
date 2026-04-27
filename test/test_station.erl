-module(test_station).

-include_lib("eunit/include/eunit.hrl").

-define(withStation(StationID, Tests),
    {foreach, setup_station(StationID), fun teardown_station/1, Tests}
).
-define(withStationX(TestsX),
    {foreachx, fun(StationID) -> setup_station(StationID, []) end,
        fun(StationID, _) -> teardown_station(StationID) end, TestsX}
).
-define(withConnectedStation(StationID, InState, Tests),
    {foreach, fun() -> setup_connected_station(StationID, InState) end,
        fun teardown_connected_station/1, Tests}
).
-define(mockStationManager(Tests),
    {setup, fun setup_station_manager_mock/0, fun teardown_station_manager_mock/1, Tests}
).

setup_station(StationID) ->
    fun() -> setup_station(StationID, []) end.

setup_deps() ->
    application:ensure_all_started(gproc).

teardown_deps(_) ->
    application:stop(gproc).

setup_station_manager_mock() ->
    ok = meck:new(ocpp_station_manager),
    meck:expect(
        ocpp_station_manager,
        connect,
        fun(_, Versions) -> {ok, lists:max(Versions)} end
    ).

teardown_station_manager_mock(_) ->
    meck:unload(ocpp_station_manager).

setup_station(StationID, Options) ->
    {ok, _Pid} = ocpp_station:start_link(StationID),
    StationID.

teardown_station(StationID) ->
    catch ocpp_station:stop(StationID).

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

%% [x]
connect_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
            ?withStation(
                <<"foo">>,
                [
                    fun connect_invalid_station/1,
                    fun connect_station/1,
                    fun connect_station_again/1,
                    fun connect_station_twice/1
                ]
            )
        )}.

%% [x]
down_test_() ->
    %% TODO these tests should succeed no matter what state the station is in.
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager([
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
        ])}.

%% [x]
connect_rejected_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
            ?withStation(
                <<"connection_rejected_test">>,
                [
                    fun connection_rejected/1,
                    fun connection_handler_error/1
                ]
            )
        )}.

%% [x]
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

%% [x]
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

%% [x]
connect_invalid_station(_StationID) ->
    {"connecting to a station ID that does not exist fails",
        ?_assertError(nostation, ocpp_station:connect(<<"invalid">>, ['2.0.1']))}.

%% [x]
connect_station(StationID) ->
    {"can connect to a station", fun() ->
        {ok, '2.0.1'} = ocpp_station:connect(StationID, ['2.0.1'])
    end}.

%% [x]
connect_station_again(StationID) ->
    {"the same process can't connect to a station twice", fun() ->
        {ok, '2.1'} = ocpp_station:connect(StationID, ['1.6', '2.0.1', '2.1']),
        ?assertMatch({error, already_connected}, ocpp_station:connect(StationID, ['2.1']))
    end}.

%% [x]
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

%% [x]
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

%% [x]
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

%% [x]
rpc_from_wrong_process_test_() ->
    %% TODO expand this to all other states
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
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
            )
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
            stopped
    end.

conn_control(StationID) ->
    receive
        {connect, From, Versions} ->
            {Pid, Version} = connect(StationID, Versions),
            From ! {ok, Version},
            conn_control(StationID, Pid, Version);
        stop ->
            ok
    after 1000 ->
        error(timeout)
    end.

conn_control(StationID, Pid, Version) ->
    receive
        {'DOWN', _, process, Pid, Info} when Info =/= normal ->
            error({client_failed, Info});
        {do, From, Fun} ->
            Pid ! {do, From, Fun},
            conn_control(StationID, Pid, Version);
        disconnect ->
            stop_client(Pid),
            conn_control(StationID);
        stop ->
            %% stop the client, we wait for the 'DOWN' message to transition states
            stop_client(Pid),
            self() ! stop,
            conn_control(StationID);
        {exit, Reason} ->
            exit(Pid, Reason);
        Message ->
            error({unexpected_message, Message})
    end.

stop_client(Pid) ->
    Pid ! stop,
    receive
        {'DOWN', _, process, Pid, normal} ->
            ok
    end.

connect_client(Pid, Versions) ->
    Pid ! {connect, self(), Versions},
    receive
        {ok, Ver} -> Ver
    after 1000 ->
        error(timeout)
    end.

disconnect_client(Pid) ->
    Pid ! disconnect.

do_client(Pid, Fun) ->
    Pid ! {do, self(), Fun},
    receive
        {result, Result} ->
            Result;
        Other ->
            error({unexpected_result, Other})
    end.

ocpp_client_recv(Timeout) ->
    receive
        {ocpp, {rpcsend, Reply}} ->
            Reply;
        Message ->
            error({unexpected_message, Message})
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

boot_to(StationID, State) ->
    boot_to(StationID, State, []).

boot_to(StationID, State, Extra) ->
    Conn = spawn_link(fun() -> conn_control(StationID) end),
    Version = connect_client(Conn, ['2.0.1']),
    ConnState = {Conn, StationID, Version},
    boot_to_(State, ConnState, Extra),
    ConnState.

boot_to_(accepted, Conn = {_Pid, _StationID, Version}, Extra) ->
    %% Can get away with this because boot notifications are the same in all three versions
    {ok, BootNotificationRequest} = ocpp_message:new(Version, ~"BootNotificationRequest", #{
        reason => 'PowerUp', chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
    }),
    {ok, BootNotificationResponse} = ocpp_message:new(Version, ~"BootNotificationResponse", #{
        status => 'Accepted', currentTime => {{2025, 12, 12}, {1, 2, 3}}, interval => 0
    }),
    do_call_response(Conn, [{BootNotificationRequest, BootNotificationResponse} | Extra]);
boot_to_(rejected, Conn = {_Pid, _StationID, Version}, Extra) ->
    {ok, BootNotificationRequest} = ocpp_message:new(Version, ~"BootNotificationRequest", #{
        reason => 'PowerUp', chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
    }),
    {ok, BootNotificationResponse} = ocpp_message:new(Version, ~"BootNotificationResponse", #{
        status => 'Rejected', currentTime => {{2025, 12, 12}, {1, 2, 3}}, interval => 0
    }),
    do_call_response(Conn, [{BootNotificationRequest, BootNotificationResponse} | Extra]);
boot_to_(pending, Conn = {_Pid, _StationID, Version}, Extra) ->
    {ok, BootNotificationRequest} = ocpp_message:new(Version, ~"BootNotificationRequest", #{
        reason => 'PowerUp', chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
    }),
    {ok, BootNotificationResponse} = ocpp_message:new(Version, ~"BootNotificationResponse", #{
        status => 'Pending', currentTime => {{2025, 12, 12}, {1, 2, 3}}, interval => 0
    }),
    do_call_response(Conn, [{BootNotificationRequest, BootNotificationResponse} | Extra]);
boot_to_(connected, Conn, Extra) ->
    do_call_response(Conn, Extra);
boot_to_(provisioning, Conn = {_, _, Version}, Extra) ->
    {ok, BootNotificationRequest} = ocpp_message:new(Version, ~"BootNotificationRequest", #{
        reason => 'PowerUp', chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
    }),
    do_call_response(Conn, [{BootNotificationRequest, undefined} | Extra]);
boot_to_(offline, Conn = {Pid, _, Version}, Extra) ->
    {ok, BootNotificationRequest} = ocpp_message:new(Version, ~"BootNotificationRequest", #{
        reason => 'PowerUp', chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
    }),
    {ok, BootNotificationResponse} = ocpp_message:new(Version, ~"BootNotificationResponse", #{
        status => 'Accepted', currentTime => {{2025, 12, 12}, {1, 2, 3}}, interval => 0
    }),
    do_call_response(Conn, [{BootNotificationRequest, BootNotificationResponse} | Extra]),
    disconnect_client(Pid);
boot_to_(reconnecting, Conn = {Pid, _, Version}, Extra) ->
    {ok, BootNotificationRequest} = ocpp_message:new(Version, ~"BootNotificationRequest", #{
        reason => 'PowerUp', chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
    }),
    {ok, BootNotificationResponse} = ocpp_message:new(Version, ~"BootNotificationResponse", #{
        status => 'Accepted', currentTime => {{2025, 12, 12}, {1, 2, 3}}, interval => 0
    }),
    do_call_response(Conn, [{BootNotificationRequest, BootNotificationResponse} | Extra]),
    disconnect_client(Pid),
    connect_client(Pid, ['2.0.1']).

unique_message_id() ->
    integer_to_binary(erlang:unique_integer()).

do_call_response({Pid, StationID, Version}, Messages) ->
    F = fun() ->
        lists:foreach(
            fun
                ({Request, undefined}) ->
                    Call = ocpp_rpc:call(Request, unique_message_id()),
                    ok = ocpp_station:rpc(StationID, ocpp_rpc:encode(Call));
                ({Request, Response}) ->
                    Call = ocpp_rpc:call(Request, unique_message_id()),
                    ok = ocpp_station:rpc(StationID, ocpp_rpc:encode(Call)),
                    ok = ocpp_station:reply(StationID, ocpp_rpc:id(Call), Response),
                    {ok, {callresult, _}} = ocpp_rpc:decode(Version, ocpp_client_recv(100), [
                        {expected, ocpp_message:action(Request)}
                    ])
            end,
            Messages
        )
    end,
    do_client(Pid, F).

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

%% [x]
%% test behavior of ocpp_station with respect to station initiated
%% messages.
provision_station_client_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
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
                                element(
                                    2,
                                    ocpp_message:new(
                                        '2.0.1',
                                        ~"BootNotificationResponse",
                                        #{
                                            status => 'Pending',
                                            interval => 1,
                                            currentTime => {{2025, 1, 1}, {0, 0, 0}}
                                        }
                                    )
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
                            {ok, ExpectedMessage} =
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"BootNotificationResponse",
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
            )
        )}.

provisioning_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
            ?withStationX([
                rpc_before_connect(),
                client_boot_retry(),
                duplicate_boot_notification_request()
            ])
        )}.

boot_pending_disallowed_test_() ->
    {"messages that are disallow while in boot pending state",
        {setup, fun setup_deps/0, fun teardown_deps/1,
            ?mockStationManager(
                ?withStationX([
                    %% Test all messages that are initiated by the station and some messages
                    %% that should not be (i.e. are normally initiated by the CSMS)
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"AuthorizeRequest",
                                    #{idToken => #{idToken => <<"">>, type => 'NoAuthorization'}}
                                )
                            ),
                            <<"auth1">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"ClearedChargingLimitRequest", #{
                                    chargingLimitSource => 'CSO'
                                })
                            ),
                            <<"clearchglimit">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"DataTransferRequest", #{
                                    vendorId => <<"bar">>
                                })
                            ),
                            <<"datatx">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"Get15118EVCertificateRequest",
                                    #{
                                        iso15118SchemaVersion => <<"invalid">>,
                                        action => 'Install',
                                        exiRequest => base64:encode("not a valid request")
                                    }
                                )
                            ),
                            <<"15118certrequest">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"GetCertificateStatusRequest",
                                    #{
                                        ocspRequestData =>
                                            #{
                                                hashAlgorithm => 'SHA256',
                                                issuerNameHash =>
                                                    <<
                                                        "61f91e29ed59e9dee56466cea5e52852"
                                                        "2cb7e59719f7bb4f7d1e633e1d261611"
                                                    >>,
                                                issuerKeyHash =>
                                                    <<
                                                        "61f91e29ed59e9dee56466cea5e528522cb7e"
                                                        "59719f7bb4f7d1e633e1d261611"
                                                    >>,
                                                serialNumber => ~"deadbeef",
                                                responderURL => <<"https://foo.com/cert">>
                                            }
                                    }
                                )
                            ),
                            <<"certstatus">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"NotifyChargingLimitRequest",
                                    #{chargingLimit => #{chargingLimitSource => 'Other'}}
                                )
                            ),
                            <<"chglimit">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"NotifyCustomerInformationRequest",
                                    #{
                                        data => <<"foo">>,
                                        seqNo => 0,
                                        generatedAt => {{2025, 1, 1}, {12, 12, 12}},
                                        requestId => 1
                                    }
                                )
                            ),
                            <<"custinfo">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"NotifyDisplayMessagesRequest",
                                    #{requestId => 1}
                                )
                            ),
                            <<"notifydispmsg">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"NotifyEVChargingNeedsRequest",
                                    #{
                                        evseId => 1,
                                        chargingNeeds =>
                                            #{requestedEnergyTransfer => 'DC'}
                                    }
                                )
                            ),
                            <<"evchgneeds">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"NotifyEVChargingScheduleRequest",
                                    #{
                                        timeBase => {{2025, 1, 1}, {18, 12, 12}},
                                        evseId => 1,
                                        chargingSchedule =>
                                            #{
                                                id => 1,
                                                chargingRateUnit => 'W',
                                                chargingSchedulePeriod => [
                                                    #{startPeriod => 0, limit => 123.4}
                                                ]
                                            }
                                    }
                                )
                            ),
                            <<"evchgsched">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"NotifyEventRequest", #{
                                    generatedAt => {{2025, 12, 12}, {16, 16, 16}},
                                    seqNo => 0,
                                    eventData => [
                                        #{
                                            eventId => 1,
                                            timestamp => {{2025, 12, 12}, {16, 16, 0}},
                                            trigger => 'Alerting',
                                            actualValue => <<"foo">>,
                                            eventNotificationType => 'CustomMonitor',
                                            component => #{name => <<"test">>},
                                            variable => #{name => <<"VarName">>}
                                        }
                                    ]
                                })
                            ),
                            <<"notifyevent">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"NotifyMonitoringReportRequest", #{
                                    requestId => 1,
                                    seqNo => 0,
                                    generatedAt => {{2025, 1, 12}, {10, 14, 12}}
                                })
                            ),
                            <<"notifymon">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"NotifyReportRequest", #{
                                    requestId => 1,
                                    generatedAt => {{2025, 1, 1}, {23, 12, 12}},
                                    seqNo => 0
                                })
                            ),
                            <<"norifyreport">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"ReportChargingProfilesRequest", #{
                                    requestId => 1,
                                    chargingLimitSource => 'CSO',
                                    evseId => 0,
                                    chargingProfile => [
                                        #{
                                            id => 1,
                                            stackLevel => 0,
                                            chargingProfilePurpose => 'TxProfile',
                                            chargingProfileKind => 'Absolute',
                                            chargingSchedule => [
                                                #{
                                                    id => 1,
                                                    chargingRateUnit => 'A',
                                                    chargingSchedulePeriod => [
                                                        #{startPeriod => 0, limit => 25.6}
                                                    ]
                                                }
                                            ]
                                        }
                                    ]
                                })
                            ),
                            <<"reportchgprof">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"ReservationStatusUpdateRequest", #{
                                    reservationId => 1, reservationUpdateStatus => 'Expired'
                                })
                            ),
                            <<"resvstatus">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"SecurityEventNotificationRequest", #{
                                    type => <<"InvalidFirmwareSigningCertificate">>,
                                    timestamp => {{2025, 1, 2}, {1, 2, 3}}
                                })
                            ),
                            <<"secevent">>
                        )
                    ),
                    %% The following disallowed messages are those that may be solicited via
                    %% a TriggerMessageRequest (with the exception of a BootNotification
                    %% which is allowed). All of these messages are disallowed when they
                    %% have not been solicited. Another test will check that they are
                    %% allowed after they have been solicited.
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new('2.0.1', ~"FirmwareStatusNotificationRequest", #{
                                    status => 'Downloaded'
                                })
                            ),
                            <<"fwstatus">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(2, ocpp_message:new('2.0.1', ~"HeartbeatRequest", #{})),
                            <<"hb1">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"LogStatusNotificationRequest",
                                    #{status => 'BadMessage'}
                                )
                            ),
                            <<"logstatus">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"MeterValuesRequest",
                                    #{
                                        evseId => 1,
                                        meterValue => [
                                            #{
                                                timestamp => {{2025, 1, 1}, {0, 0, 0}},
                                                sampledValue => [#{value => 1.0}]
                                            }
                                        ]
                                    }
                                )
                            ),
                            <<"metervals">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"SignCertificateRequest",
                                    #{
                                        csr => <<
                                            "-----BEGIN CERTIFICATE REQUEST-----\n"
                                            "MIIDGDCCAgACAQAwgakxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpDYWxpZm9ybmlh\n"
                                            "MRYwFAYDVQQHEw1Nb3VudGFpbiBWaWV3MRYwFAYDVQQKEw1Hb29nbGUsIEluYy4g\n"
                                            "MRcwFQYDVQQLEw5JVCBEZXB0YXJ0bWVudDEXMBUGA1UEAxMOd3d3Lmdvb2dsZS5j\n"
                                            "b20xIzAhBgkqhkiG9w0BCQEWFHdlYm1hc3RlckBnb29nbGUuY29tMIIBIjANBgkq\n"
                                            "hkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAq3NT5DBBDql5gTB4/6Zsq/C1iwO4yBD2\n"
                                            "nThaNfO1qHKUjnFz0oua+54x97TjmHItRH5H+jPJvmzzb4TUJ274CRFhquOOMZVM\n"
                                            "dVIG9FUjogJstMqv4GtBC4C/ype0ilAcPEBjRi9bFiR/g43qPCnlRAJNo4cJko7n\n"
                                            "W7erAJsRPNiQMr5UJN9h3GuQMPw6uaI/0OWuWjSTLzEBMujHhPySgZIv1SurVXDz\n"
                                            "iFC6S6qvc9XQ1z6tkmrttdoOfDI+eT75QxysHmctgAvkZaFEoRASqcqf3iYyl9Qw\n"
                                            "mh0xuLSoR9HTvaD9DhxAIa4/1+l6D9MGb/01+lip7AjqdnTTzSBfcQIDAQABoCkw\n"
                                            "JwYJKoZIhvcNAQkOMRowGDAJBgNVHRMEAjAAMAsGA1UdDwQEAwIF4DANBgkqhkiG\n"
                                            "9w0BAQsFAAOCAQEAZyMkFtElkS3vQoCPVHevrFcPgrx/Fqx0UdQdnf2RyoJ3jqiU\n"
                                            "yPo5+5BHA9kY0TuJLhgMIq0QWAbzZYNL0+J8UUcx8EvMK6DqPpKteyYFCMw6GEzu\n"
                                            "diq4RE/8Ea9UpGbw8GH1oEsUksBTwrs06OSOVgDXkJ1XY4VaRkMPflgQWGULgKYO\n"
                                            "2P/zcFowENruGLJO7ynyUkm5idKdYzDqk7c7bqyLywOEPxSRKVyblmzqiFCOlCqp\n"
                                            "HozZ9+5TmrMPD/hO1uHVECcL08RMGXoGMajojI8CE+cmkaWLq3PZt08Sv0F/Itop\n"
                                            "O8XAZ2bYTK4HQfPm+Fud22SD+DkSwt8vN8Lu2g==\n"
                                            "-----END CERTIFICATE REQUEST-----\n"
                                        >>
                                    }
                                )
                            ),
                            <<"signcert">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"StatusNotificationRequest",
                                    #{
                                        timestamp => {{2025, 1, 1}, {1, 1, 1}},
                                        evseId => 1,
                                        connectorId => 1,
                                        connectorStatus => 'Available'
                                    }
                                )
                            ),
                            <<"staus">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"TransactionEventRequest",
                                    #{
                                        eventType => 'Started',
                                        timestamp => {{2025, 1, 1}, {12, 12, 12}},
                                        triggerReason => 'CablePluggedIn',
                                        seqNo => 0,
                                        transactionInfo =>
                                            #{transactionId => <<"tx1">>}
                                    }
                                )
                            ),
                            <<"txevent">>
                        )
                    ),
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"PublishFirmwareStatusNotificationRequest",
                                    #{status => 'Idle'}
                                )
                            ),
                            <<"pubfwstatus">>
                        )
                    ),
                    %% this message is normally sent by the CSMS to the charging station;
                    %% however, we should still reject it even though it would normally
                    %% result in a NotSupportedError (instead of a SecurityError).
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"DeleteCertificateRequest",
                                    #{
                                        certificateHashData =>
                                            #{
                                                hashAlgorithm => 'SHA256',
                                                issuerNameHash =>
                                                    <<
                                                        "61f91e29ed59e9dee56466cea5e52852"
                                                        "2cb7e59719f7bb4f7d1e633e1d261611"
                                                    >>,
                                                issuerKeyHash =>
                                                    <<
                                                        "61f91e29ed59e9dee56466cea5e528522cb7e"
                                                        "59719f7bb4f7d1e633e1d261611"
                                                    >>,
                                                serialNumber => <<"deadbeef">>
                                            }
                                    }
                                )
                            ),
                            <<"delcert">>
                        )
                    ),
                    %% As for DeleteCertificateRequest, this would normally result in a
                    %% NotSupportedError; however, here it results in a SecurityError I want
                    %% to test this message specifically because this is a message that is
                    %% expected to be sent to the station while in the pending state.
                    %% Allowing the send from the CSMS should not allow the (invalid)
                    %% message from the station.
                    pending_disallowed(
                        ocpp_rpc:call(
                            element(
                                2,
                                ocpp_message:new(
                                    '2.0.1',
                                    ~"GetBaseReportRequest",
                                    #{requestId => 1, reportBase => 'FullInventory'}
                                )
                            ),
                            <<"basereport">>
                        )
                    )
                ])
            )}}.

boot_pending_trigger_message_test_() ->
    {"the CSMS may solicit messages using a TriggerMessageRequest from the boot_pending state",
        {setup, fun setup_deps/0, fun teardown_deps/1,
            ?mockStationManager(
                {inorder,
                    ?withStationX(
                        [
                            pending_trigger(
                                element(
                                    2,
                                    ocpp_message:new(
                                        '2.0.1',
                                        ~"TriggerMessageRequest",
                                        #{requestedMessage => 'BootNotification'}
                                    )
                                ),
                                element(
                                    2,
                                    ocpp_message:new('2.0.1', ~"BootNotificationRequest", #{
                                        reason => 'Triggered',
                                        chargingStation => #{
                                            model => <<"foo">>, vendorName => <<"bar">>
                                        }
                                    })
                                ),
                                element(
                                    2,
                                    ocpp_message:new('2.0.1', ~"BootNotificationResponse", #{
                                        status => 'Pending',
                                        interval => 0,
                                        currentTime => {{2025, 12, 12}, {3, 2, 1}}
                                    })
                                )
                            ),
                            pending_trigger(
                                element(
                                    2,
                                    ocpp_message:new(
                                        '2.0.1',
                                        ~"TriggerMessageRequest",
                                        #{requestedMessage => 'Heartbeat'}
                                    )
                                ),
                                element(2, ocpp_message:new('2.0.1', ~"HeartbeatRequest", #{})),
                                element(
                                    2,
                                    ocpp_message:new(
                                        '2.0.1',
                                        ~"HeartbeatResponse",
                                        #{currentTime => {{2025, 12, 12}, {1, 1, 1}}}
                                    )
                                )
                            ),
                            pending_trigger(
                                element(
                                    2,
                                    ocpp_message:new(
                                        '2.0.1',
                                        ~"TriggerMessageRequest",
                                        #{
                                            requestedMessage => 'StatusNotification',
                                            evse => #{id => 1, connectorId => 1}
                                        }
                                    )
                                ),
                                element(
                                    2,
                                    ocpp_message:new(
                                        '2.0.1',
                                        ~"StatusNotificationRequest",
                                        #{
                                            timestamp => {{2025, 1, 1}, {1, 1, 1}},
                                            evseId => 1,
                                            connectorId => 1,
                                            connectorStatus => 'Available'
                                        }
                                    )
                                ),
                                element(
                                    2, ocpp_message:new('2.0.1', ~"StatusNotificationResponse", #{})
                                )
                            )
                            %% TODO add the rest of the triggerable messages
                            %% TODO add test for attempt to send a response to a message
                            %%      that has benn retried
                        ]
                    )}
            )}}.

pending_triggered_rejected_test_() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
            ?withStation(
                StationID,
                [pending_triggered_rejected(StationID)]
            )
        )}.

pending_triggered_rejected(StationID) ->
    {setup, local, fun() -> boot_to_pending(StationID) end,
        {"A triggered message that was rejected should be dissallowed from a station in the pending state",
            fun() ->
                MsgID = integer_to_binary(erlang:unique_integer()),
                {ok, TriggerMessageRequest} =
                    ocpp_message:new(
                        '2.0.1',
                        ~"TriggerMessageRequest",
                        #{requestedMessage => 'Heartbeat'}
                    ),
                ok = ocpp_station:call(StationID, MsgID, TriggerMessageRequest),
                {ok, {call, TriggerCall}} = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), []),
                ?assertEqual(TriggerMessageRequest, ocpp_rpc:payload(TriggerCall)),
                {ok, TriggerMessageResponse} =
                    ocpp_message:new(
                        '2.0.1',
                        ~"TriggerMessageResponse",
                        #{status => 'Rejected'}
                    ),
                ok = ocpp_station:rpc(
                    StationID, ocpp_rpc:encode(ocpp_rpc:callresult(TriggerMessageResponse, MsgID))
                ),
                HBID = integer_to_binary(erlang:unique_integer()),
                {ok, HeartbeatRequest} =
                    ocpp_message:new('2.0.1', ~"HeartbeatRequest", #{}),
                ocpp_station:rpc(StationID, ocpp_rpc:encode(ocpp_rpc:call(HeartbeatRequest, HBID))),
                HBResult = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), [
                    {expected, <<"Heartbeat">>}
                ]),
                ?assertMatch({ok, {callerror, _}}, HBResult),
                {ok, {callerror, CallError}} = HBResult,
                ?assertEqual('SecurityError', ocpp_rpc:error_code(CallError)),
                ?assertEqual(
                    <<"Disallowed or unsolicited CALL while boot pending">>,
                    ocpp_rpc:error_description(CallError)
                ),
                ?assertMatch(#{<<"action">> := <<"Heartbeat">>}, ocpp_rpc:error_details(CallError))
            end}}.

pending_triggered_duplicate_test_() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
            ?withStation(
                StationID,
                [pending_triggered_duplicate(StationID)]
            )
        )}.

pending_triggered_duplicate(StationID) ->
    {setup, local, fun() -> boot_to_pending(StationID) end,
        {"A triggered message that was accepted should be allowed only once a station in the pending state",
            fun() ->
                MsgID = integer_to_binary(erlang:unique_integer()),
                {ok, TriggerMessageRequest} =
                    ocpp_message:new(
                        '2.0.1',
                        ~"TriggerMessageRequest",
                        #{requestedMessage => 'Heartbeat'}
                    ),
                ok = ocpp_station:call(StationID, MsgID, TriggerMessageRequest),
                {ok, {call, TriggerCall}} = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), []),
                ?assertEqual(TriggerMessageRequest, ocpp_rpc:payload(TriggerCall)),
                {ok, TriggerMessageResponse} =
                    ocpp_message:new(
                        '2.0.1',
                        ~"TriggerMessageResponse",
                        #{status => 'Accepted'}
                    ),
                ok = ocpp_station:rpc(
                    StationID, ocpp_rpc:encode(ocpp_rpc:callresult(TriggerMessageResponse, MsgID))
                ),
                HBID = integer_to_binary(erlang:unique_integer()),
                {ok, HeartbeatRequest} =
                    ocpp_message:new('2.0.1', ~"HeartbeatRequest", #{}),
                ocpp_station:rpc(StationID, ocpp_rpc:encode(ocpp_rpc:call(HeartbeatRequest, HBID))),
                {ok, HeartbeatResponse} =
                    ocpp_message:new('2.0.1', ~"HeartbeatResponse", #{
                        currentTime => {{2025, 1, 1}, {0, 0, 0}}
                    }),
                ok = ocpp_station:reply(StationID, HBID, HeartbeatResponse),
                HBResult = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), [
                    {expected, <<"Heartbeat">>}
                ]),
                CallResult = ocpp_rpc:callresult(HeartbeatResponse, HBID),
                ?assertEqual({ok, {callresult, CallResult}}, HBResult),
                HBID1 = integer_to_binary(erlang:unique_integer()),
                ocpp_station:rpc(
                    StationID, ocpp_rpc:encode(ocpp_rpc:call(HeartbeatRequest, HBID1))
                ),
                HBResult1 = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), [
                    {expected, <<"Heartbeat">>}
                ]),
                ?assertMatch({ok, {callerror, _}}, HBResult1),
                {ok, {callerror, CallError}} = HBResult1,
                ?assertEqual('SecurityError', ocpp_rpc:error_code(CallError)),
                ?assertEqual(
                    <<"Disallowed or unsolicited CALL while boot pending">>,
                    ocpp_rpc:error_description(CallError)
                ),
                ?assertMatch(#{<<"action">> := <<"Heartbeat">>}, ocpp_rpc:error_details(CallError))
            end}}.

pending_untriggered_boot_notification_test_() ->
    {"station in pending state sends a BootNotificationRequest without being triggered",
        {setup, fun setup_deps/0, fun teardown_deps/1,
            ?mockStationManager(
                ?withStationX([untriggered_boot_normal(), untriggered_boot_racing()])
            )}}.

untriggered_boot_normal() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {StationID, fun(_, _) ->
        {setup, local, fun() -> boot_to_pending(StationID) end,
            {"station sends an unsolicited BootNotificationRequest while in pending state", fun() ->
                    MsgID = integer_to_binary(erlang:unique_integer()),
                    BootNotificationRequest = default_boot_notification_request(MsgID),
                    ok = ocpp_station:rpc(StationID, BootNotificationRequest),
                    {ok, BootNotificationResponse} =
                        ocpp_message:new(
                            '2.0.1',
                            ~"BootNotificationResponse",
                            #{
                                status => 'Accepted',
                                interval => 0,
                                currentTime => {{2025, 1, 1}, {1, 2, 4}}
                            }
                        ),
                    ok = ocpp_station:reply(StationID, MsgID, BootNotificationResponse),
                    Response = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), [
                        {expected, <<"BootNotification">>}
                    ]),
                    ?assertMatch({ok, {callresult, _}}, Response),
                    {ok, {callresult, CallResult}} = Response,
                    ?assertEqual(MsgID, ocpp_rpc:id(CallResult)),
                    ?assertEqual(BootNotificationResponse, ocpp_rpc:payload(CallResult)),
                    %% Send a heartbeat to prove that we are out of the pending state
                    HBID = integer_to_binary(erlang:unique_integer()),
                    ok = ocpp_station:rpc(
                        StationID,
                        ocpp_rpc:encode(
                            ocpp_rpc:call(
                                element(2, ocpp_message:new('2.0.1', ~"HeartbeatRequest", #{})),
                                HBID
                            )
                        )
                    ),
                    {ok, HB} =
                        ocpp_message:new('2.0.1', ~"HeartbeatResponse", #{
                            currentTime => {{2025, 1, 1}, {1, 3, 0}}
                        }),
                    ok = ocpp_station:reply(StationID, HBID, HB),
                    HBResponse = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), [
                        {expected, <<"Heartbeat">>}
                    ]),
                    ?assertMatch({ok, {callresult, _}}, HBResponse),
                    {ok, {callresult, HBCallResult}} = HBResponse,
                    ?assertEqual(HBID, ocpp_rpc:id(HBCallResult)),
                    ?assertEqual(HB, ocpp_rpc:payload(HBCallResult))
                end}}
    end}.

untriggered_boot_racing() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {StationID, fun(_, _) ->
        {setup, local, fun() -> boot_to_pending(StationID) end,
            {"boot notification after request to trigger boot notification, but before trigger response",
                fun() ->
                    BootID = integer_to_binary(erlang:unique_integer()),
                    TriggerID = integer_to_binary(erlang:unique_integer()),
                    {ok, TriggerMessageRequest} =
                        ocpp_message:new(
                            '2.0.1',
                            ~"TriggerMessageRequest",
                            #{requestedMessage => 'BootNotification'}
                        ),
                    ok = ocpp_station:call(StationID, TriggerID, TriggerMessageRequest),
                    TriggerMessageReceived = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), []),
                    ?assertMatch({ok, {call, _}}, TriggerMessageReceived),
                    {ok, {call, TriggeredRPC}} = TriggerMessageReceived,
                    ?assertEqual(TriggerID, ocpp_rpc:id(TriggeredRPC)),
                    ?assertEqual(TriggerMessageRequest, ocpp_rpc:payload(TriggeredRPC)),
                    {ok, BootNotificationRequest} =
                        ocpp_message:new(
                            '2.0.1',
                            ~"BootNotificationRequest",
                            #{
                                reason => 'PowerUp',
                                chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
                            }
                        ),
                    ok = ocpp_station:rpc(
                        StationID, ocpp_rpc:encode(ocpp_rpc:call(BootNotificationRequest, BootID))
                    ),
                    %% NOTE the order of these messages and receives is important. By
                    %%      testing in this order we can be assured that the CSMS will
                    %%      respond to the BootNotificationRequest even if it has not
                    %%      received a response to its TriggerMessageRequest. This is
                    %%      important because when the station sends a spontaneous
                    %%      BootNotificationRequest it is the result of the
                    %%      CSMS-specified interval elapsing. In this case the station
                    %%      has effectively restarted the provisioning process and it
                    %%      may be waiting for a response to its boot notificaion before
                    %%      responding to the trigger message request or it may even
                    %%      drop the trigger message request (this scenario is not
                    %%      totally clear to me, but it seems like there are very few
                    %%      constraints so the CSMS should be as flexible as
                    %%      possible---i.e. don't block waiting for the trigger message
                    %%      request to time out).
                    {ok, BootNotificationResponse} =
                        ocpp_message:new(
                            '2.0.1',
                            ~"BootNotificationResponse",
                            #{
                                status => 'Accepted',
                                currentTime => {{2025, 1, 1}, {2, 2, 2}},
                                interval => 0
                            }
                        ),
                    ok = ocpp_station:reply(StationID, BootID, BootNotificationResponse),
                    BootResponse = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), [
                        {expected, <<"BootNotification">>}
                    ]),
                    ?assertMatch({ok, {callresult, _}}, BootResponse),
                    {ok, {callresult, BootResponseRPC}} = BootResponse,
                    ?assertEqual(BootID, ocpp_rpc:id(BootResponseRPC)),
                    ?assertEqual(BootNotificationResponse, ocpp_rpc:payload(BootResponseRPC)),
                    ok = ocpp_station:rpc(
                        StationID,
                        ocpp_rpc:encode(
                            ocpp_rpc:callresult(
                                element(
                                    2,
                                    ocpp_message:new('2.0.1', ~"TriggerMessageResponse", #{
                                        status => 'Rejected'
                                    })
                                ),
                                TriggerID
                            )
                        )
                    )
                end}}
    end}.

boot_to_pending(StationID) ->
    {ok, '2.0.1'} = ocpp_station:connect(StationID, ['2.0.1']),
    ocpp_station:rpc(
        StationID,
        default_boot_notification_request(<<"1">>)
    ),
    {ok, BootNotificationResponse} =
        ocpp_message:new(
            '2.0.1',
            ~"BootNotificationResponse",
            #{
                status => 'Pending',
                interval => 0,
                currentTime => {{2025, 1, 1}, {1, 2, 3}}
            }
        ),
    ocpp_station:reply(
        StationID,
        <<"1">>,
        BootNotificationResponse
    ),
    Msg = ocpp_client_recv(100),
    ExpectedRPC = ocpp_rpc:callresult(BootNotificationResponse, <<"1">>),
    {ok, {callresult, ExpectedRPC}} = ocpp_rpc:decode('2.0.1', Msg, [
        {expected, <<"BootNotification">>}
    ]).

pending_trigger(TriggerMessageRequest, Message, Response) ->
    Prefix = atom_to_binary(?FUNCTION_NAME),
    Action = ocpp_message:action(Message),
    {<<Prefix/binary, "_", Action/binary>>, fun(StationID, _) ->
        {setup, local, fun() -> boot_to_pending(StationID) end,
            {"trigger " ++ binary_to_list(Action) ++ " from pending station", fun() ->
                MsgID = <<"trigger">>,
                %% tell the station to send the TriggerMessageRequest
                ok = ocpp_station:call(StationID, MsgID, TriggerMessageRequest),
                Result1 = ocpp_client_recv(100),
                Decoded1 = ocpp_rpc:decode('2.0.1', Result1, []),
                ?assertMatch({ok, {call, _}}, Decoded1),
                {ok, {call, Call}} = Decoded1,
                ?assertEqual(MsgID, ocpp_rpc:id(Call)),
                ?assertEqual(TriggerMessageRequest, ocpp_rpc:payload(Call)),
                %% Send TriggerMessageResponse
                ?assertEqual(
                    ok,
                    ocpp_station:rpc(
                        StationID,
                        ocpp_rpc:encode(
                            ocpp_rpc:callresult(
                                element(
                                    2,
                                    ocpp_message:new('2.0.1', ~"TriggerMessageResponse", #{
                                        status => 'Accepted'
                                    })
                                ),
                                MsgID
                            )
                        )
                    )
                ),
                %% Send the triggered message
                TriggeredRPC = ocpp_rpc:call(Message, <<"triggered">>),
                ?assertEqual(ok, ocpp_station:rpc(StationID, ocpp_rpc:encode(TriggeredRPC))),
                %% Tell the station to respond
                ok = ocpp_station:reply(
                    StationID,
                    <<"triggered">>,
                    Response
                ),
                Result2 = ocpp_client_recv(100),
                Decoded2 = ocpp_rpc:decode('2.0.1', Result2, [{expected, Action}]),
                ?assertMatch({ok, {callresult, _}}, Decoded2),
                {ok, {callresult, CallResult}} = Decoded2,
                ?assertEqual(<<"triggered">>, ocpp_rpc:id(CallResult)),
                ?assertEqual(Response, ocpp_rpc:payload(CallResult))
            end}}
    end}.

pending_disallowed(RPC) ->
    Prefix = atom_to_binary(?FUNCTION_NAME),
    Action = ocpp_message:action(ocpp_rpc:payload(RPC)),
    {<<Prefix/binary, "_", Action/binary>>, fun(StationID, _) ->
        {setup, local, fun() -> boot_to_pending(StationID) end,
            {binary_to_list(Action) ++ " not allowed in pending state", fun() ->
                ok = ocpp_station:rpc(StationID, ocpp_rpc:encode(RPC)),
                Result = ocpp_client_recv(100),
                Decoded = ocpp_rpc:decode('2.0.1', Result, [{expected, <<"Heartbeat">>}]),
                ?assertMatch({ok, {callerror, _}}, Decoded),
                {ok, {callerror, CallError}} = Decoded,
                ?assertEqual(ocpp_rpc:id(RPC), ocpp_rpc:id(CallError)),
                ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
                ?assertEqual('SecurityError', ocpp_rpc:error_code(CallError)),
                ?assertEqual(
                    <<"Disallowed or unsolicited CALL while boot pending">>,
                    ocpp_rpc:error_description(CallError)
                ),
                ?assertMatch(#{<<"action">> := Action}, ocpp_rpc:error_details(CallError))
            end}}
    end}.

duplicate_boot_notification_request() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {StationID, fun(_, _) -> duplicate_boot_notification_request(StationID) end}.

duplicate_boot_notification_request(StationID) ->
    {"a repeated BootNotificationRequest RPCCALL with the same message ID id rejected", fun() ->
        {ok, _} = ocpp_station:connect(StationID, ['2.0.1']),
        ok = ocpp_station:rpc(StationID, default_boot_notification_request(<<"1">>)),
        ?assertMatch(
            {error, duplicate_message},
            ocpp_station:rpc(StationID, default_boot_notification_request(<<"1">>))
        ),
        %% previous message is processed normally
        {ok, BootNotificationResponse} =
            ocpp_message:new(
                '2.0.1',
                ~"BootNotificationResponse",
                #{
                    status => 'Accepted',
                    interval => 1,
                    currentTime => {{2025, 1, 1}, {0, 0, 1}}
                }
            ),
        ocpp_station:reply(
            StationID,
            <<"1">>,
            BootNotificationResponse
        ),
        Msg = ocpp_client_recv(1000),
        ?assertEqual(
            {ok, {callresult, ocpp_rpc:callresult(BootNotificationResponse, <<"1">>)}},
            ocpp_rpc:decode('2.0.1', Msg, [{expected, <<"BootNotification">>}])
        )
    end}.

client_boot_retry() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {StationID, fun(_, _) -> client_boot_retry(StationID) end}.

default_boot_notification_request(ID) ->
    <<
        ~S<[2,">,
        ID/binary,
        ~S<","BootNotification",{"reason":"PowerUp",>,
        ~S<"chargingStation":{"model":"a","vendorName":"b"}}]>
    >>.

client_boot_retry(StationID) ->
    {
        "when the station retries a BootNotification the pending notification "
        "is discarded and the new notification is pocessed",
        fun() ->
            {ok, _} = ocpp_station:connect(StationID, ['2.0.1']),
            ok = ocpp_station:rpc(
                StationID,
                default_boot_notification_request(<<"1">>)
            ),
            ok = ocpp_station:rpc(
                StationID,
                default_boot_notification_request(<<"2">>)
            ),
            %% "delayed" response to timed out boot notification request
            {ok, Resp} =
                ocpp_message:new(
                    '2.0.1',
                    ~"BootNotificationResponse",
                    #{
                        status => 'Accepted',
                        interval => 1,
                        currentTime => {{2025, 1, 1}, {0, 0, 0}}
                    }
                ),

            ocpp_station:reply(
                StationID,
                <<"1">>,
                Resp
            ),
            %% Make sure the station is still not provisioned
            ?assertEqual(
                {error, not_provisioned},
                ocpp_station:rpc(
                    StationID,
                    ~B<[2,"3","Heartbeat",{}]>
                )
            ),
            {ok, BootNotificationResponse} =
                ocpp_message:new(
                    '2.0.1',
                    ~"BootNotificationResponse",
                    #{
                        status => 'Accepted',
                        interval => 1,
                        currentTime => {{2025, 1, 1}, {0, 0, 1}}
                    }
                ),
            ocpp_station:reply(
                StationID,
                <<"2">>,
                BootNotificationResponse
            ),
            Msg = ocpp_client_recv(1000),
            ?assertEqual(
                {ok, {callresult, ocpp_rpc:callresult(BootNotificationResponse, <<"2">>)}},
                ocpp_rpc:decode('2.0.1', Msg, [{expected, <<"BootNotification">>}])
            ),
            ?assertError(timeout, ocpp_client_recv(50))
        end
    }.

rpc_before_connect() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {StationID, fun(_, _) -> rpc_before_connect(StationID) end}.

rpc_before_connect(StationID) ->
    {"rpc before station is connected results in {error, not_connected}", fun() ->
        Result = ocpp_station:rpc(
            StationID,
            <<
                ~S<[2,"id","BootNotification",{"reason":"PowerUp",>,
                ~S<"chargingStation":{"model":"a","vendorName":"b"}}]>
            >>
        ),
        ?assertEqual({error, not_connected}, Result)
    end}.

boot_rejected_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?mockStationManager(
            ?withStationX(
                [call_rejected_station()]
            )
        )}.

call_rejected_station() ->
    StationID = atom_to_binary(?FUNCTION_NAME),
    {StationID, fun(_, _) ->
        {"The CSMS SHALL NOT initiate any message to a station it has rejected", fun() ->
            {ok, '2.0.1'} = ocpp_station:connect(StationID, ['2.0.1']),
            BootNotificationRequest = ocpp_rpc:encode(
                ocpp_rpc:call(
                    element(
                        2,
                        ocpp_message:new('2.0.1', ~"BootNotificationRequest", #{
                            reason => 'PowerUp',
                            chargingStation => #{model => <<"a">>, vendorName => <<"b">>}
                        })
                    ),
                    <<"RejectMe">>
                )
            ),
            ok = ocpp_station:rpc(StationID, BootNotificationRequest),
            {ok, Response} =
                ocpp_message:new(
                    '2.0.1',
                    ~"BootNotificationResponse",
                    #{
                        status => 'Rejected',
                        currentTime => {{2025, 1, 1}, {12, 3, 4}},
                        interval => 0
                    }
                ),
            ok = ocpp_station:reply(StationID, <<"RejectMe">>, Response),
            BootNotificationResponse = ocpp_rpc:decode('2.0.1', ocpp_client_recv(100), [
                {expected, <<"BootNotification">>}
            ]),
            ?assertEqual(
                {ok, {callresult, ocpp_rpc:callresult(Response, <<"RejectMe">>)}},
                BootNotificationResponse
            ),
            %% station remains connected, but is compliant and does not send messages
            {ok, AttemptedMsg} =
                ocpp_message:new(
                    '2.0.1',
                    ~"GetBaseReportRequest",
                    #{
                        requestId => 1,
                        reportBase => 'FullInventory'
                    }
                ),
            ?assertEqual(
                {error, not_provisioned},
                ocpp_station:call(StationID, <<"IllegalCall">>, AttemptedMsg)
            ),
            ?assertError(timeout, ocpp_client_recv(100))
        end}
    end}.

%% unexpected_rpc_test_() ->
%%     {setup, fun setup_deps/0, fun teardown_deps/1,
%%         ?mockStationManager(
%%             ?withStationX(
%%                 [
%%                     unexpected_callresult(Version, State)
%%                  || State <- [
%%                         connected, provisioning, pending, accepted, offline, reconnecting, rejected
%%                     ],
%%                     Version <- ['1.6', '2.0.1', '2.1']
%%                 ]
%%             )
%%         )}.

%% bad_callresult_payload_test_() ->
%%     {setup, fun setup_deps/0, fun teardown_deps/1,
%%         ?mockStationManager(
%%             ?withStationX(
%%                 [wrong_callresult_payload(State) || State <- [pending, accepted, reconnecting]]
%%             )
%%         )}.

unexpected_callresult(Version, State) ->
    TestID = atom_to_binary(?FUNCTION_NAME),
    StateID = atom_to_binary(State),
    {<<TestID/binary, "_", StateID/binary>>, fun(StationID, _) ->
        {setup, fun() -> boot_to(StationID, State) end, fun teardown_conn/1, fun(
            {Pid, _, _Version}
        ) ->
            {
                "(" ++ atom_to_list(Version) ++
                    ") unexpected CALLRESULT in state '" ++ binary_to_list(StateID) ++ "'",
                fun() ->
                    {ok, Message} = ocpp_message:new(Version, ~"CancelReservationResponse", #{
                        status => 'Accepted'
                    }),
                    RPC = ocpp_rpc:callresult(Message, unique_message_id()),
                    F = fun() -> ocpp_station:rpc(StationID, ocpp_rpc:encode(RPC)) end,
                    case do_client(Pid, F) of
                        Result when
                            State =:= connected;
                            State =:= rejected
                        ->
                            ?assertEqual({error, not_provisioned}, Result);
                        Result when State =:= offline ->
                            ?assertEqual({error, not_connected}, Result);
                        Result when
                            Version =:= '2.1',
                            (State =:= accepted) orelse
                                (State =:= reconnecting) orelse
                                (State =:= provisioning) orelse
                                (State =:= pending)
                        ->
                            ?assertEqual(ok, Result),
                            ok = ocpp_station:callresulterror(
                                StationID, ocpp_rpc:id(RPC), 'InternalError', [
                                    {description, <<"Unknown message ID">>},
                                    {data, {}}
                                ]
                            ),
                            RecvFun = fun() -> ocpp_client_recv(100) end,
                            RecvResult = do_client(Pid, RecvFun),
                            ?assertMatch({rpcsend, _}, RecvResult),
                            {rpcsend, Bin} = RecvResult,
                            Decoded = ocpp_rpc:decode('2.1', Bin, []),
                            ?assertMatch({ok, {callresulterror, _}}, Decoded),
                            {ok, {callresulterror, CallResultError}} = Decoded,
                            ?assertEqual(ocpp_rpc:id(RPC), ocpp_rpc:id(CallResultError)),
                            ?assertEqual('InternalError', ocpp_rpc:error_code(CallResultError));
                        Result when
                            State =:= accepted;
                            State =:= reconnecting;
                            State =:= provisioning;
                            State =:= pending
                        ->
                            %% TODO verify that no rpcsend message is received.
                            ?assertEqual(ok, Result),
                            ?assertEqual(
                                {error, cannot_send},
                                ocpp_station:callresulterror(
                                    StationID, ocpp_rpc:id(RPC), 'InternalError', [
                                        {description, <<"Unknown message ID">>},
                                        {data, #{}}
                                    ]
                                )
                            ),
                            TimeoutFun = fun() ->
                                try
                                    ocpp_client_recv(50)
                                catch
                                    error:timeout -> timeout
                                end
                            end,
                            ?assertEqual(timeout, do_client(Pid, TimeoutFun))
                    end
                end
            }
        end}
    end}.

wrong_callresult_payload(State) ->
    {<<"asdf">>, fun(_, _) -> fun() -> ?assert(false) end end}.

%% TODO Things to test:

%% 1. for all states after receiving a BootNotificationRequest that is not (yet)
%%    rejected:
%%    - [X] a new RPC CALL comes in with a DIFFERENT message ID - this indicates that
%%          the station timed out waiting for the reply and has either sent a new
%%          message or retried the timed out message. Expected behavior: processing of
%%          the old message is abandoned and the new message is processed.
%%    - [X] a new RPC CALL comes in with the SAME message ID - this is an error and
%%          should not happen; however, we should plan to handle it. Expected behavior:
%%          drop the message.

%% 2. [x] BootNotificationRequest while a previous BootNotificaionRequest is being
%%        processed. The most likely cause of this would be that handler has failed and
%%        the station has timed out waiting for a response. Expectation should be to
%%        drop the old request and process the new one. DONE in (1)

%% 3. [x] Any unsolicited message while boot is pending, other than another
%%        BootNotificationRequest (after the designated interval, if specified in the
%%        previous BootNotificationResponse) SHALL result in a CALLERROR: SecurityError
%%        (Requirement B01.FR.10, B02.FR.09, & B03.FR.07)

%% 4. [x] The CSMS SHALL NOT send any messages to a station it has rejected. I think
%%        this generalizes to sending any message to the station before a
%%        BootNotificationResponse has been sent with status Accepted or Pending

%% 6. [x] If the station rejects the TriggerMessageRequest, then the requested message
%%        should still be disallowed. If no response arrives and the requested message
%%        is received the message should NOT be considered a triggered message.

%% 7. BootNotificaionRequest arrives at some time after a
%%    BootNotificationResponse(status=Accepted) has been sent but
%%    before any other messages arrive from the station. This
%%    indicates that the response never arrived at the station. We
%%    should reset to a provisioning state and send a new response.

%% 8. BootNotificationRequest arrives at some time after the station
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

%% 9. Incoming CALLRESULT that does not match the expected type (may or may not match
%%    the expected message ID) [provisioning_call_pending state and ready_call_pending
%%    state]

%% 10. Incoming CALL of same type as triggered call, but before the
%%     TriggerMessageResponse is sent. These should be rejected as if not solicited. A
%%     subsequent message, folloing the TriggerMessageResponse, must be accepted.
