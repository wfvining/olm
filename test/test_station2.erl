-module(test_station2).

-include_lib("eunit/include/eunit.hrl").

-define(withStation(Tests),
    {foreach, fun start_station/0, fun teardown_station/1, Tests}
).

%% @doc This is meant to be used as an argument to `withStation/1`. It
%% must be provided a list of Test objects which are each wrapped
%% first in an instanitator function which takes the station ID as its
%% only parameter and then in a `setup` fixture that creates a client
%% for the station ID. The result of the seupt fixture is a tuple
%% `{ClientPid, StationID}` which will be passed to instantiators
%% contained in `Tests`
-define(withClient(Tests), [
    fun(StationID) ->
        {setup, fun() -> setup_client(StationID) end, fun teardown_client/1, Test}
    end
 || Test <- Tests
]).

%% @doc Setup a single client for running all tests. `Client` is a
%% pattern that will be bound to the return value from setup_client/1.
%% (i.e. a {ClientPid, StationID} tuple).
-define(setupClient(Client, Tests), fun(StationID) ->
    {setup, fun() -> setup_client(StationID) end, fun teardown_client/1, fun(Client) -> Tests end}
end).

%% @doc Setup a connection with a station. Expects to be placed in the
%% body of a `setupClient` TODO the use of `Client` in this macro
%% assumes that the variable exists and is bound to ther return value of `setup_client/1`. That's not great. I need to fix that.
-define(withConnection(Client, ConnHandle, Versions, Description, TestBody),
    {setup, fun() -> ocpp_test_client:connect(Client, Versions) end,
        fun ocpp_test_client:disconnect/1, fun(ConnHandle) ->
            {Description, fun() ->
                TestBody
            end}
        end}
).

connect_test_() ->
    {"check basic connect functionality", setup, fun mock_station_manager/0,
        fun teardown_station_manager/1,
        ?withStation(
            [
                fun connect_invalid_station/1,
                fun connect_station/1,
                fun connect_station_again/1,
                fun connect_station_twice/1
            ]
        )}.

reconnect_test_() ->
    {"test reconnect after client process disconnects", setup, fun mock_station_manager/0,
        fun teardown_station_manager/1,
        ?withStation(
            ?withClient(
                lists:concat([
                    [reconnect_voluntary(State, Version), reconnect_killed(State, Version)]
                 || State <- [connected, provisioning, pending, rejected, accepted, reconnecting],
                    Version <- ['1.6', '2.0.1', '2.1']
                ])
            )
        )}.

connect_rejected_test_() ->
    {setup,
        fun() ->
            meck:new(ocpp_station_manager),
            meck:expect(ocpp_station_manager, connect, fun
                (_, ['1.6']) -> {ok, '1.6'};
                (_, ['2.1']) -> close;
                (_, ['2.0.1']) -> {error, because}
            end)
        end,
        fun(_) -> meck:unload() end,
        ?withStation(
            [
                fun(StationID) ->
                    {"connection rejected by handler can succeed later", fun() ->
                        ?assertEqual(close, ocpp_station:connect(StationID, ['2.1'])),
                        ?assertEqual({ok, '1.6'}, ocpp_station:connect(StationID, ['1.6'])),
                        ?assertEqual(
                            {error, already_connected}, ocpp_station:connect(StationID, ['2.1'])
                        ),
                        %% same thing happens for a different process
                        {ok, Client} = ocpp_test_client:start_link(StationID),
                        ?assertEqual(
                            {error, already_connected}, ocpp_test_client:connect(Client, ['2.1'])
                        ),
                        ?assert(meck:validate(ocpp_station_manager)),
                        ocpp_test_client:stop(Client)
                    end}
                end,
                fun(StationID) ->
                    {"connection rejected when handler fails", fun() ->
                        ?assertEqual({error, because}, ocpp_station:connect(StationID, ['2.0.1'])),
                        ?assertEqual(close, ocpp_station:connect(StationID, ['2.1'])),
                        ?assertEqual({ok, '1.6'}, ocpp_station:connect(StationID, ['1.6'])),
                        ?assert(meck:validate(ocpp_station_manager))
                    end}
                end
            ]
        )}.

rpc_call_from_wrong_process_test_() ->
    {setup, fun mock_station_manager/0, fun teardown_station_manager/1,
        ?withStation(
            ?withClient(
                [
                    fun({Client, StationID}) ->
                        ?withConnection(
                            Client,
                            Conn,
                            [Version],
                            "(" ++ atom_to_list(Version) ++
                                ") RPC from process that is not connected is rejected in state '" ++
                                atom_to_list(State) ++ "'",
                            begin
                                if
                                    State =/= unprovisioned, State =/= offline ->
                                        ocpp_test_client:boot_to(Conn, State),
                                        Request = legal_request(State, Version);
                                    State =:= offline ->
                                        ocpp_test_client:boot_to(Conn, State),
                                        Request = ocpp_test_client:make_boot_notification_request(
                                            Version, []
                                        );
                                    State =:= unprovisioned ->
                                        Request = ocpp_test_client:make_boot_notification_request(
                                            Version, []
                                        )
                                end,
                                MessageID = ocpp_test_client:message_id(),
                                RPC = ocpp_rpc:call(Request, MessageID),
                                ?assertEqual(
                                    {error, not_connected},
                                    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPC))
                                )
                            end
                        )
                    end
                 || State <- [
                        unprovisioned,
                        connected,
                        provisioning,
                        pending,
                        accepted,
                        rejected,
                        reconnecting,
                        offline
                    ],
                    Version <- ['1.6', '2.0.1', '2.1']
                ]
            )
        )}.

provision_station_test_() ->
    {setup, fun mock_station_manager/0, fun teardown_station_manager/1, fun(_) ->
        [
            [
                ?withStation(
                    ?withClient(
                        [
                            fun({Client, StationID}) ->
                                ?withConnection(
                                    Client,
                                    ConnHandle,
                                    [Version],
                                    "(" ++ atom_to_list(Version) ++
                                        ") CALL prior to BootNotificationRequest is rejected",
                                    call_before_boot(ConnHandle, StationID, Version)
                                )
                            end
                        ]
                    )
                ),
                ?withStation(
                    ?withClient(
                        [
                            fun({Client, StationID}) ->
                                ?withConnection(
                                    Client,
                                    ConnHandle,
                                    [Version],
                                    "(" ++ atom_to_list(Version) ++
                                        ") CSMS cannot CALL station prior to boot",
                                    call_station_before_boot(ConnHandle, StationID, Version)
                                )
                            end
                        ]
                    )
                ),
                ?withStation(
                    ?withClient(
                        [
                            fun({Client, StationID}) ->
                                ?withConnection(
                                    Client,
                                    ConnHandle,
                                    [Version],
                                    "(" ++ atom_to_list(Version) ++
                                        ") CALL before BootNotificationResponse is rejected",
                                    no_call_while_provisioning(ConnHandle, StationID, Version)
                                )
                            end
                        ]
                    )
                ),
                ?withStation(
                    ?withClient(
                        [
                            fun({Client, StationID}) ->
                                ?withConnection(
                                    Client,
                                    ConnHandle,
                                    [Version],
                                    "(" ++ atom_to_list(Version) ++
                                        ") repeat BootNotificationRequest before "
                                        "BootNotificationResponse is allowed",
                                    repeat_boot_notification_request_while_provisioning(
                                        ConnHandle, StationID, Version
                                    )
                                )
                            end
                        ]
                    )
                ),
                ?withStation(
                    ?withClient(
                        [
                            fun({Client, StationID}) ->
                                ?withConnection(
                                    Client,
                                    ConnHandle,
                                    [Version],
                                    "(" ++ atom_to_list(Version) ++
                                        ") 'Pending' response puts station in state "
                                        "that allows calls to station",
                                    boot_pending_call_station(ConnHandle, StationID, Version)
                                )
                            end
                        ]
                    )
                ),
                ?withStation(
                    ?withClient(
                        [
                            fun({Client, StationID}) ->
                                ?withConnection(
                                    Client,
                                    ConnHandle,
                                    [Version],
                                    "(" ++ atom_to_list(Version) ++
                                        ") 'Pending' response puts station in state "
                                        "that allows repeat BootNotificationRequest",
                                    boot_pending_repeat_boot_notification(
                                        ConnHandle, StationID, Version
                                    )
                                )
                            end
                        ]
                    )
                )
            ]
         || Version <- ['1.6', '2.0.1', '2.1']
        ]
    end}.

%% TODO boot_pending_trigger_message_test_()
%% TODO boot_pending_triggered_duplicate_test_() untriggered_normal, untriggered_racing, triggered
boot_pending_trigger_message_test_() ->
    {setup, fun mock_station_manager/0, fun teardown_station_manager/1, [
        boot_pending_untriggered(),
        boot_pending_triggered_racing(),
        boot_pending_triggered_rejected(),
        boot_pending_triggered_accepted()
    ]}.

boot_pending_untriggered() ->
    [
        ?withStation(
            ?withClient(
                [
                    fun({Client, StationID}) ->
                        ?withConnection(
                            Client,
                            ConnHandle,
                            [Version],
                            "(" ++ atom_to_list(Version) ++
                                ") untriggered CALL rejected while in 'pending' state",
                            boot_pending_untriggered(ConnHandle, StationID, Version)
                        )
                    end
                ]
            )
        )
     || Version <- ['1.6', '2.0.1', '2.1']
    ].

boot_pending_untriggered(Conn, StationID, Version) ->
    ocpp_test_client:boot_to(Conn, pending),
    StatusNotificationRequest = status_notification(Version),
    ID = ocpp_test_client:message_id(),
    Result = ocpp_test_client:do(
        Conn,
        fun() ->
            ocpp_station:rpc(
                StationID,
                ocpp_rpc:encode(
                    ocpp_rpc:call(StatusNotificationRequest, ID)
                )
            )
        end
    ),
    if
        Version =:= '1.6' ->
            ?assertEqual({error, not_provisioned}, Result),
            ?assertError(
                timeout, ocpp_test_client:do(Conn, fun() -> ocpp_test_client:recv(100) end)
            );
        Version =/= '1.6' ->
            Response = ocpp_test_client:do(
                Conn,
                fun() -> ocpp_test_client:recv(100) end
            ),
            ?assertMatch({ok, {callerror, _}}, ocpp_rpc:decode(Version, Response, [])),
            {ok, {callerror, CallError}} = ocpp_rpc:decode(Version, Response, []),
            ?assertEqual(ID, ocpp_rpc:id(CallError)),
            ?assertEqual('SecurityError', ocpp_rpc:error_code(CallError))
    end.

boot_pending_triggered_racing() ->
    [
        [
            boot_pending_untriggered_boot_racing(Version)
        ]
     || Version <- ['1.6', '2.0.1', '2.1']
    ].

boot_pending_untriggered_boot_racing(Version) ->
    {inparallel, [
        boot_pending_untriggered_boot_racing(Version, BootStatus, TriggerStatus)
     || BootStatus <- ['Accepted', 'Rejected', 'Pending'],
        TriggerStatus <- ['Accepted', 'Rejected']
    ]}.

boot_pending_untriggered_boot_racing(Version, UnsolicitedBootStatus, TriggerStatus) ->
    Description =
        fun(Order) ->
            iolist_to_binary(
                io_lib:format(
                    "(~s) station responds ~p to a TriggerMessageRequest(BootNotification) "
                    "it receives after sending a spontaneous BootNotification that is ~s "
                    "by the CSMS." ++
                        if
                            Order =:= 'before' ->
                                " TriggerMessageResponse sent before the the BootNotificationResponse"
                                " and retried after.";
                            Order =:= 'after' ->
                                ""
                        end,
                    [Version, TriggerStatus, UnsolicitedBootStatus]
                )
            )
        end,
    [
        ?withStation(
            ?withClient(
                [
                    fun({Client, StationID}) ->
                        ?withConnection(
                            Client,
                            ConnHandle,
                            [Version],
                            Description(Order),
                            begin
                                ocpp_test_client:boot_to(ConnHandle, pending),
                                {TriggerMessageRequest, TriggerMessageResponse} =
                                    trigger_message_request(
                                        Version, 'BootNotification', TriggerStatus
                                    ),
                                ocpp_station:call(
                                    StationID, <<"triggerID">>, TriggerMessageRequest
                                ),
                                BootID = ocpp_test_client:message_id(),
                                %% Send a BootNotificationRequest *before* responding the the TriggerMessage
                                BootNotificationRequest =
                                    ocpp_test_client:make_boot_notification_request(Version, []),
                                BootFun =
                                    fun() ->
                                        ok = ocpp_station:rpc(
                                            StationID,
                                            ocpp_rpc:encode(
                                                ocpp_rpc:call(BootNotificationRequest, BootID)
                                            )
                                        )
                                    end,
                                ok = ocpp_test_client:do(ConnHandle, BootFun),
                                TM = ocpp_test_client:do(ConnHandle, fun() ->
                                    ocpp_test_client:recv(100)
                                end),
                                ?assertMatch({ok, {call, _}}, ocpp_rpc:decode(Version, TM, [])),
                                {ok, {call, Call}} = ocpp_rpc:decode(Version, TM, []),
                                ?assertEqual(TriggerMessageRequest, ocpp_rpc:payload(Call)),
                                ?assertEqual(<<"triggerID">>, ocpp_rpc:id(Call)),
                                UnsolicitedBootNotificationResponse =
                                    ocpp_test_client:make_boot_notification_response(
                                        Version, UnsolicitedBootStatus, []
                                    ),
                                Expected =
                                    if
                                        UnsolicitedBootStatus =:= 'Rejected' ->
                                            {error, not_provisioned};
                                        true ->
                                            ok
                                    end,
                                PostFun =
                                    fun() ->
                                        if
                                            Order =:= 'before' ->
                                                %% The station responds to the TriggerMessageRequest before
                                                %% it receives a response to its unsolicited
                                                %% BootNotificationRequest
                                                %%
                                                %% this is forbidden by B01.FR.08
                                                {error, not_provisioned} = ocpp_station:rpc(
                                                    StationID,
                                                    ocpp_rpc:encode(
                                                        ocpp_rpc:callresult(
                                                            TriggerMessageResponse, <<"triggerID">>
                                                        )
                                                    )
                                                );
                                            Order =:= 'after' ->
                                                ok
                                        end,
                                        ok = ocpp_station:reply(
                                            StationID, BootID, UnsolicitedBootNotificationResponse
                                        ),
                                        ocpp_station:rpc(
                                            StationID,
                                            ocpp_rpc:encode(
                                                ocpp_rpc:callresult(
                                                    TriggerMessageResponse, <<"triggerID">>
                                                )
                                            )
                                        )
                                    end,
                                ?assertMatch(Expected, ocpp_test_client:do(ConnHandle, PostFun)),
                                BootResponse = ocpp_test_client:do(
                                    ConnHandle, fun() -> ocpp_test_client:recv(100) end
                                ),
                                ?assertMatch(
                                    {ok, {callresult, _}},
                                    ocpp_rpc:decode(Version, BootResponse, [
                                        {expected, <<"BootNotification">>}
                                    ])
                                ),
                                {ok, {callresult, CallResult}} =
                                    ocpp_rpc:decode(Version, BootResponse, [
                                        {expected, <<"BootNotification">>}
                                    ]),
                                ?assertEqual(
                                    UnsolicitedBootNotificationResponse,
                                    ocpp_rpc:payload(CallResult)
                                ),
                                ?assertEqual(BootID, ocpp_rpc:id(CallResult)),
                                if
                                    TriggerStatus =:= 'Accepted' ->
                                        ok = ocpp_test_client:boot_to(ConnHandle, accepted);
                                    true ->
                                        %% nothing to do
                                        ok
                                end
                            end
                        )
                    end
                ]
            )
        )
     || Order <- ['after', 'before']
    ].

boot_pending_triggered_rejected() ->
    MessageTypes = #{
        '1.6' => [
            'DiagnosticsStatusNotification',
            'FirmwareStatusNotification',
            'Heartbeat',
            'MeterValues',
            'StatusNotification'
        ],
        '2.0.1' => [
            'LogStatusNotification',
            'FirmwareStatusNotification',
            'Heartbeat',
            'MeterValues',
            'SignChargingStationCertificate',
            'SignV2GCertificate',
            'StatusNotification',
            'TransactionEvent',
            'SignCombinedCertificate',
            'PublishFirmwareStatusNotification'
        ],
        '2.1' => [
            'LogStatusNotification',
            'FirmwareStatusNotification',
            'Heartbeat',
            'MeterValues',
            'SignChargingStationCertificate',
            'SignV2GCertificate',
            'StatusNotification',
            'TransactionEvent',
            'SignCombinedCertificate',
            'PublishFirmwareStatusNotification',
            'SignV2G20Certificate',
            'CustomTrigger'
        ]
    },
    [
        {inparallel, [
            ?withStation(
                ?withClient(
                    [
                        fun({Client, StationID}) ->
                            ?withConnection(
                                Client,
                                ConnHandle,
                                [Version],
                                iolist_to_binary(
                                    io_lib:format(
                                        "(~p) After rejecting a TriggerMessageRequest in the pending state, "
                                        "the station receives a SecurityError if it sends the requested "
                                        "~p message.",
                                        [Version, MessageType]
                                    )
                                ),
                                do_rejected_trigger(StationID, ConnHandle, Version, MessageType)
                            )
                        end
                    ]
                )
            )
         || MessageType <- maps:get(Version, MessageTypes)
        ]}
     || Version <- ['1.6', '2.0.1', '2.1']
    ].

do_rejected_trigger(StationID, ConnHandle, Version, MessageType) ->
    {TriggerMessageRequest, TriggerMessageResponse} =
        trigger_message_request(Version, MessageType, 'Rejected'),
    Message = make_message(Version, MessageType),
    TriggerRPCRequest = ocpp_rpc:call(TriggerMessageRequest, ocpp_test_client:message_id()),
    TriggerRPCResponse = ocpp_rpc:callresult(
        TriggerMessageResponse, ocpp_rpc:id(TriggerRPCRequest)
    ),
    ocpp_test_client:boot_to(ConnHandle, pending),
    ?assertEqual(
        ok, ocpp_station:call(StationID, ocpp_rpc:id(TriggerRPCRequest), TriggerMessageRequest)
    ),
    ?assertMatch(
        {ok, {call, _}},
        ocpp_rpc:decode(
            Version,
            ocpp_test_client:do(
                ConnHandle,
                fun() ->
                    ocpp_test_client:recv(100)
                end
            ),
            []
        )
    ),
    ?assertEqual(
        ok,
        ocpp_test_client:do(
            ConnHandle,
            fun() ->
                ocpp_station:rpc(StationID, ocpp_rpc:encode(TriggerRPCResponse))
            end
        )
    ),
    TriggeredID = ocpp_test_client:message_id(),
    TriggeredRPCCall = ocpp_rpc:encode(ocpp_rpc:call(Message, TriggeredID)),
    Res = ocpp_test_client:do(ConnHandle, fun() -> ocpp_station:rpc(StationID, TriggeredRPCCall) end),
    case Version of
        '1.6' ->
            ?assertEqual({error, not_provisioned}, Res);
        _ ->
            ?assertEqual(ok, Res),
            Response = ocpp_test_client:do(ConnHandle, fun() -> ocpp_test_client:recv(100) end),
            DecodedResponse = ocpp_rpc:decode(Version, Response, []),
            ?assertMatch({ok, {callerror, _}}, DecodedResponse),
            {ok, {callerror, CallError}} = DecodedResponse,
            ?assertEqual(TriggeredID, ocpp_rpc:id(CallError)),
            ?assertEqual('SecurityError', ocpp_rpc:error_code(CallError))
    end.

boot_pending_triggered_accepted() ->
    %% 1. triggered messages are allowed - normal CALLRESULT is received
    %% 2. non-triggered message disallowed before and after triggered message
    %% 3. duplicate triggered message disallowed (? CombinedCertificate signing request)
    %% 4. multiple triggered messages
    {inparallel, [
        boot_pending_triggered_allowed(),
        boot_pending_untriggered_not_allowed(),
        boot_pending_duplicate_triggered_not_allowed(),
        boot_pending_multiple_triggers_allowed()
    ]}.

boot_pending_triggered_allowed() ->
    MessageTypes = #{
        '1.6' => [
            'DiagnosticsStatusNotification',
            'FirmwareStatusNotification',
            'Heartbeat',
            'MeterValues',
            'StatusNotification'
        ],
        '2.0.1' => [
            'LogStatusNotification',
            'FirmwareStatusNotification',
            'Heartbeat',
            'MeterValues',
            'SignChargingStationCertificate',
            'SignV2GCertificate',
            'StatusNotification',
            'TransactionEvent',
            'SignCombinedCertificate',
            'PublishFirmwareStatusNotification'
        ],
        '2.1' => [
            'LogStatusNotification',
            'FirmwareStatusNotification',
            'Heartbeat',
            'MeterValues',
            'SignChargingStationCertificate',
            'SignV2GCertificate',
            'StatusNotification',
            'TransactionEvent',
            'SignCombinedCertificate',
            'PublishFirmwareStatusNotification',
            'SignV2G20Certificate',
            'CustomTrigger'
        ]
    },
    [
        [
            ?withStation(
                ?withClient(
                    [
                        fun({Client, StationID}) ->
                            ?withConnection(
                                Client,
                                ConnHandle,
                                [Version],
                                iolist_to_binary(
                                    io_lib:format(
                                        "(~p) After rejecting a TriggerMessageRequest in the pending state, "
                                        "the station receives a SecurityError if it sends the requested "
                                        "~p message.",
                                        [Version, MessageType]
                                    )
                                ),
                                do_rejected_trigger(StationID, ConnHandle, Version, MessageType)
                            )
                        end
                    ]
                )
            )
         || MessageType <- maps:get(Version, MessageTypes)
        ]
     || Version <- ['1.6', '2.0.1', '2.1']
    ].

boot_pending_untriggered_not_allowed() ->
    %{"bpuna", ?_assert(false)}.
    [].
boot_pending_duplicate_triggered_not_allowed() ->
    %{"bpdtna", ?_assert(false)}.
    [].
boot_pending_multiple_triggers_allowed() ->
    %{"bpmta", ?_assert(false)}.
    [].

%% TODO unexpected_callresult_test_()
%% TODO bad_callresult_payload_test_()
%% TODO bad_callresult_message_id_test_()

%%% test functions

connect_invalid_station(_StationID) ->
    {"connecting to a station ID that does not exist fails", [
        ?_assert(_StationID =/= <<"invalid-station">>),
        ?_assertError(nostation, ocpp_station:connect(<<"invalid-station">>, ['2.0.1']))
    ]}.

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

reconnect_voluntary(State, Version) ->
    fun({Client, StationID}) ->
        ?withConnection(
            Client,
            Conn,
            [Version],
            "(" ++ atom_to_list(Version) ++ ") can reconnect after voluntary disconnect in state '" ++
                atom_to_list(State) ++ "'",
            begin
                ocpp_test_client:boot_to(Conn, State),
                ocpp_test_client:disconnect(Conn),
                ocpp_test_client:connect(Client, [Version]),
                ?assertEqual(Version, ocpp_test_client:version(Client)),
                ?assertEqual(
                    {error, already_connected},
                    ocpp_station:connect(StationID, [Version])
                )
            end
        )
    end.

reconnect_killed(State, Version) ->
    fun({Client, StationID}) ->
        ?withConnection(
            Client,
            ConnHandle,
            [Version],
            "(" ++ atom_to_list(Version) ++ ") can reconnect after involuntay disconnect in state '" ++
                atom_to_list(State) ++ "'",
            begin
                ocpp_test_client:boot_to(ConnHandle, State),
                ocpp_test_client:disconnect(ConnHandle, kill),
                ocpp_test_client:connect(Client, [Version]),
                ?assertEqual(Version, ocpp_test_client:version(Client)),
                ?assertEqual(
                    {error, already_connected},
                    ocpp_station:connect(StationID, [Version])
                )
            end
        )
    end.

call_before_boot(Conn, StationID, Version) ->
    {ok, Request} = ocpp_message:new(Version, ~"HeartbeatRequest", #{}),
    F = fun() ->
        RPC = ocpp_rpc:call(Request, ocpp_test_client:message_id()),
        ocpp_station:rpc(StationID, ocpp_rpc:encode(RPC))
    end,
    ?assertEqual({error, not_provisioned}, ocpp_test_client:do(Conn, F)),
    ?assertError(timeout, ocpp_test_client:do(Conn, fun() -> ocpp_test_client:recv(50) end)).

call_station_before_boot(ConnHandle, StationID, Version) ->
    {ResetRequest, _ResetResponse} = reset_request(Version),
    ?assertEqual(
        {error, not_provisioned},
        ocpp_station:call(StationID, ocpp_test_client:message_id(), ResetRequest)
    ),
    ?assertError(timeout, ocpp_test_client:do(ConnHandle, fun() -> ocpp_test_client:recv(50) end)).

boot_pending_call_station(ConnHandle, StationID, Version) ->
    ocpp_test_client:boot_to(ConnHandle, pending),
    {TriggerMessageRequest, TriggerMessageResponse} = trigger_message_request(Version),
    MsgID = ocpp_test_client:message_id(),
    SendFun = fun() -> ocpp_test_client:recv(100) end,
    ReplyFun = fun() ->
        ocpp_station:rpc(
            StationID, ocpp_rpc:encode(ocpp_rpc:callresult(TriggerMessageResponse, MsgID))
        )
    end,
    ?assertEqual(ok, ocpp_station:call(StationID, MsgID, TriggerMessageRequest)),
    RPC = ocpp_test_client:do(ConnHandle, SendFun),
    ?assertMatch({ok, {call, _}}, ocpp_rpc:decode(Version, RPC, [])),
    {ok, {call, Call}} = ocpp_rpc:decode(Version, RPC, []),
    ?assertEqual(MsgID, ocpp_rpc:id(Call)),
    ?assertEqual(TriggerMessageRequest, ocpp_rpc:payload(Call)),
    ?assertEqual(ok, ocpp_test_client:do(ConnHandle, ReplyFun)).

boot_pending_repeat_boot_notification(ConnHandle, StationID, Version) ->
    ocpp_test_client:boot_to(ConnHandle, pending),
    BootNotificationRequest = ocpp_test_client:make_boot_notification_request(Version, []),
    BootNotificationResponse = ocpp_test_client:make_boot_notification_response(
        Version, 'Pending', []
    ),
    MsgID = ocpp_test_client:message_id(),
    ?assertEqual(
        ok,
        ocpp_test_client:do(
            ConnHandle,
            fun() ->
                RPC = ocpp_rpc:encode(ocpp_rpc:call(BootNotificationRequest, MsgID)),
                ocpp_station:rpc(StationID, RPC)
            end
        )
    ),
    %% make sure we can't send calls now
    %%
    %% XXX The correct behavior here is not clear, regardless, we can
    %% test it on its own in a different test.
    %%
    %% F = fun() ->
    %%     {ok, Req} = ocpp_message:new(Version, ~"HeartbeatRequest", #{}),
    %%     ocpp_station:rpc(
    %%         StationID, ocpp_rpc:encode(ocpp_rpc:call(Req, ocpp_test_client:message_id()))
    %%     )
    %% end,
    %% ?assertEqual({error, not_provisioned}, ocpp_test_client:do(ConnHandle, F)),

    %% return to the pending state
    ocpp_station:reply(StationID, MsgID, BootNotificationResponse),
    Response = ocpp_test_client:do(ConnHandle, fun() -> ocpp_test_client:recv(100) end),
    ?assertMatch(
        {ok, {callresult, _}},
        ocpp_rpc:decode(Version, Response, [{expected, <<"BootNotification">>}])
    ),
    {ok, {callresult, CallResult}} = ocpp_rpc:decode(Version, Response, [
        {expected, <<"BootNotification">>}
    ]),
    ?assertEqual(BootNotificationResponse, ocpp_rpc:payload(CallResult)),
    ?assertEqual(MsgID, ocpp_rpc:id(CallResult)),

    %% check that we behave like we are in the pending state
    {TriggerMessageRequest, TriggerMessageResponse} = trigger_message_request(Version),
    MsgID1 = ocpp_test_client:message_id(),
    SendFun = fun() -> ocpp_test_client:recv(100) end,
    ReplyFun = fun() ->
        ocpp_station:rpc(
            StationID, ocpp_rpc:encode(ocpp_rpc:callresult(TriggerMessageResponse, MsgID1))
        )
    end,
    ?assertEqual(ok, ocpp_station:call(StationID, MsgID1, TriggerMessageRequest)),
    RPC = ocpp_test_client:do(ConnHandle, SendFun),
    ?assertMatch({ok, {call, _}}, ocpp_rpc:decode(Version, RPC, [])),
    {ok, {call, Call}} = ocpp_rpc:decode(Version, RPC, []),
    ?assertEqual(MsgID1, ocpp_rpc:id(Call)),
    ?assertEqual(TriggerMessageRequest, ocpp_rpc:payload(Call)),
    ?assertEqual(ok, ocpp_test_client:do(ConnHandle, ReplyFun)).

no_call_while_provisioning(Conn, StationID, Version) ->
    {ok, Request} = ocpp_message:new(Version, ~"HeartbeatRequest", #{}),
    F = fun() ->
        RPC = ocpp_rpc:call(Request, ocpp_test_client:message_id()),
        ocpp_station:rpc(StationID, ocpp_rpc:encode(RPC))
    end,
    ok = ocpp_test_client:boot_to(Conn, provisioning),
    ?assertEqual({error, not_provisioned}, ocpp_test_client:do(Conn, F)),
    ?assertError(timeout, ocpp_test_client:do(Conn, fun() -> ocpp_test_client:recv(50) end)).

repeat_boot_notification_request_while_provisioning(Conn, StationID, Version) ->
    BootNotificationRequest = ocpp_test_client:make_boot_notification_request(Version, []),
    BootNotificationResponse = ocpp_test_client:make_boot_notification_response(
        Version, 'Accepted', []
    ),
    ID1 = ocpp_test_client:message_id(),
    ID2 = ocpp_test_client:message_id(),
    F = fun() ->
        ok = ocpp_station:rpc(
            StationID, ocpp_rpc:encode(ocpp_rpc:call(BootNotificationRequest, ID1))
        ),
        ok = ocpp_station:rpc(
            StationID, ocpp_rpc:encode(ocpp_rpc:call(BootNotificationRequest, ID2))
        ),
        ocpp_station:reply(StationID, ID1, BootNotificationResponse)
    end,
    ?assertEqual({error, {call_not_pending, ID1}}, ocpp_test_client:do(Conn, F)),
    ?assertError(timeout, ocpp_test_client:do(Conn, fun() -> ocpp_test_client:recv(50) end)),
    no_call_while_provisioning(Conn, StationID, Version).

%%% fixtures and utility functions

station_id() ->
    integer_to_binary(erlang:unique_integer([positive]), 36).

-doc """
Creat a new station. The StationID is returned.
""".
start_station() ->
    %% ensure gproc is started, we aren't going to shut it down
    %% between tests since we guarantee unique station IDs here so it
    %% may already be running.
    {ok, _} = application:ensure_all_started(gproc),
    StationID = station_id(),
    {ok, _} = ocpp_station:start_link(StationID),
    StationID.

teardown_station(StationID) ->
    ocpp_station:stop(StationID).

%%% useful mocks
-doc """
Create a mock of the ocpp_station_manager module. Specifically, the
ocpp_station_manager:connect/2 function is patched to enable
connections. The patch always accepts the connection at the highest
OCPP version proferred.
""".
mock_station_manager() ->
    ok = meck:new(ocpp_station_manager),
    meck:expect(ocpp_station_manager, connect, fun(_, Versions) -> {ok, lists:max(Versions)} end).

teardown_station_manager(_) ->
    meck:unload(ocpp_station_manager).

-doc """
Spawn and link to and OCPP client process for `StationID`. Returns the
Pid of the client.
""".
setup_client(StationID) ->
    {ok, Client} = ocpp_test_client:start_link(StationID),
    {Client, StationID}.

-doc """
Disconnect and shut down an OCPP client and its controlling process.
""".
teardown_client({Pid, _StationID}) ->
    ocpp_test_client:stop(Pid).

legal_request(State, '1.6') when
    State =:= connected;
    State =:= provisioning;
    State =:= pending;
    State =:= rejected;
    State =:= reconnecting
->
    element(
        2,
        ocpp_message:new(
            '1.6',
            ~"BootNotificationRequest",
            #{chargePointModel => <<"legal_request">>, chargePointVendor => <<"eunit">>}
        )
    );
legal_request(State, Version) when
    State =:= connected;
    State =:= provisioning;
    State =:= pending;
    State =:= rejected;
    State =:= reconnecting
->
    element(
        2,
        ocpp_message:new(
            Version,
            ~"BootNotificationRequest",
            #{
                reason => 'PowerUp',
                chargingStation => #{model => <<"legal_request">>, vendorName => <<"eunit">>}
            }
        )
    );
legal_request(accepted, Version) ->
    make_authorize_request(Version).

%%% version agnostic message generators

make_message(Version, 'Heartbeat') ->
    element(2, ocpp_message:new(Version, ~"HeartbeatRequest", #{}));
make_message('1.6', 'DiagnosticsStatusNotification') ->
    element(
        2,
        ocpp_message:new(
            '1.6',
            ~"DiagnosticsStatusNotificationRequest",
            #{status => 'Idle'}
        )
    );
make_message('1.6', 'FirmwareStatusNotification') ->
    element(
        2,
        ocpp_message:new(
            '1.6',
            ~"FirmwareStatusNotificationRequest",
            #{status => 'Downloaded'}
        )
    );
make_message('1.6', 'MeterValues') ->
    element(
        2, ocpp_message:new('1.6', ~"MeterValuesRequest", #{connectorId => 1, meterValue => []})
    );
make_message('1.6', 'StatusNotification') ->
    element(
        2,
        ocpp_message:new(
            '1.6',
            ~"StatusNotificationRequest",
            #{connectorId => 1, errorCode => 'NoError', status => 'Available'}
        )
    );
make_message(Version, 'LogStatusNotification') ->
    element(2, ocpp_message:new(Version, ~"LogStatusNotificationRequest", #{status => 'Idle'}));
make_message(Version, 'FirmwareStatusNotification') ->
    element(
        2,
        ocpp_message:new(Version, ~"FirmwareStatusNotificationRequest", #{status => 'Downloaded'})
    );
make_message(Version, 'MeterValues') ->
    element(
        2,
        ocpp_message:new(
            Version,
            ~"MeterValuesRequest",
            #{
                evseId => 1,
                meterValue => [
                    #{
                        timestamp => ~"2026-01-01T16:30:00Z",
                        sampledValue => [#{value => 0.0}]
                    }
                ]
            }
        )
    );
make_message(Version, Msg) when
    Msg =:= 'SignChargingStationCertificate';
    Msg =:= 'SignCombinedCertificate'
->
    element(
        2,
        ocpp_message:new(Version, ~"SignCertificateRequest", #{
            csr => <<"cert-signing-request">>, certificateType => 'ChargingStationCertificate'
        })
    );
make_message(Version, 'SignV2GCertificate') ->
    element(
        2,
        ocpp_message:new(
            Version,
            ~"SignCertificateRequest",
            #{
                csr => <<"cert signing request">>,
                certificateType => 'V2GCertificate'
            }
        )
    );
make_message(Version, 'StatusNotification') ->
    element(
        2,
        ocpp_message:new(
            Version,
            ~"StatusNotificationRequest",
            #{
                timestamp => ~"2026-01-02T15:15:00Z",
                evseId => 1,
                connectorStatus => 'Available',
                connectorId => 1
            }
        )
    );
make_message(Version, 'TransactionEvent') ->
    element(
        2,
        ocpp_message:new(
            Version,
            ~"TransactionEventRequest",
            #{
                eventType => 'Updated',
                timestamp => ~"2026-01-02T12:12:12Z",
                triggerReason => 'Trigger',
                seqNo => 1,
                transactionInfo => #{transactionId => ~"a"}
            }
        )
    );
make_message(Version, 'PublishFirmwareStatusNotification') ->
    element(
        2,
        ocpp_message:new(Version, ~"PublishFirmwareStatusNotificationRequest", #{status => 'Idle'})
    );
make_message('2.1', 'SignV2G20Certificate') ->
    element(
        2,
        ocpp_message:new(
            '2.1',
            ~"SignCertificateRequest",
            #{
                csr => <<"cert signing request">>,
                certificateType => 'V2G20Certificate'
            }
        )
    );
make_message('2.1', 'CustomTrigger') ->
    element(
        2, ocpp_message:new('2.1', ~"UnlockConnectorRequest", #{evseId => 1, connectorId => 1})
    ).

make_security_error(RPCCall) ->
    ocpp_rpc:callerror('SecurityError', ocpp_rpc:id(RPCCall), []).

make_rpc_request(Payload, MessageID) ->
    ocpp_rpc:call(Payload, MessageID).

make_authorize_request('1.6') ->
    element(2, ocpp_message:new('1.6', ~"AuthorizeRequest", #{idTag => <<"EUnitToken">>}));
make_authorize_request('2.0.1') ->
    element(
        2,
        ocpp_message:new('2.0.1', ~"AuthorizeRequest", #{
            idToken => #{idToken => <<"">>, type => 'NoAuthorization'}
        })
    );
make_authorize_request('2.1') ->
    element(
        2,
        ocpp_message:new('2.1', ~"AuthorizeRequest", #{
            idToken => #{idToken => <<"">>, type => <<"NoAuthorization">>}
        })
    ).

heartbeat_request(Version) ->
    {
        element(2, ocpp_message:new(Version, ~"HeartbeatRequest", #{})),
        element(
            2,
            ocpp_message:new(Version, ~"HeartbeatResponse", #{
                currentTime => {{2025, 10, 10}, {1, 2, 3}}
            })
        )
    }.

reset_request('1.6') ->
    {
        element(2, ocpp_message:new('1.6', ~"ResetRequest", #{type => 'Hard'})),
        element(2, ocpp_message:new('1.6', ~"ResetResponse", #{status => 'Rejected'}))
    };
reset_request(Version) ->
    {
        element(2, ocpp_message:new(Version, ~"ResetRequest", #{type => 'Immediate'})),
        element(2, ocpp_message:new(Version, ~"ResetResponse", #{status => 'Rejected'}))
    }.

trigger_message_request(Version) ->
    {
        element(
            2,
            ocpp_message:new(Version, ~"TriggerMessageRequest", #{requestedMessage => 'Heartbeat'})
        ),
        element(2, ocpp_message:new(Version, ~"TriggerMessageResponse", #{status => 'Rejected'}))
    }.

trigger_message_request('2.1', 'CustomTrigger', Status) ->
    {
        element(
            2,
            ocpp_message:new(
                '2.1',
                ~"TriggerMessageRequest",
                #{requestedMessage => 'CustomTrigger', customTrigger => <<"UnlockConnector">>}
            )
        ),
        element(2, ocpp_message:new('2.1', ~"TriggerMessageResponse", #{status => Status}))
    };
trigger_message_request(Version, MessageType, Status) ->
    {
        element(
            2,
            ocpp_message:new(Version, ~"TriggerMessageRequest", #{requestedMessage => MessageType})
        ),
        element(2, ocpp_message:new(Version, ~"TriggerMessageResponse", #{status => Status}))
    }.

status_notification('1.6') ->
    element(
        2,
        ocpp_message:new(
            '1.6',
            ~"StatusNotificationRequest",
            #{connectorId => 0, errorCode => 'NoError', status => 'Available'}
        )
    );
status_notification(Version) ->
    element(
        2,
        ocpp_message:new(Version, ~"StatusNotificationRequest", #{
            timestamp => {{2025, 1, 1}, {1, 1, 1}},
            connectorStatus => 'Available',
            connectorId => 1,
            evseId => 1
        })
    ).
