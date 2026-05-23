-module(prop_ocpp201_fsm).
-eqwalizer(ignore).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    initial_state/0,
    initial_state_data/0,
    weight/3,
    precondition/4,
    postcondition/5,
    next_state_data/5
]).
-export([
    offline/1,
    connected/1,
    booting/1,
    pending/1,
    idle/1,
    idle/2
]).
-export([todo/1]).

-define(STATIONID, atom_to_binary(?MODULE)).

prop_test() ->
    ?SETUP(
        fun() ->
            {ok, Apps} = application:ensure_all_started(gproc),
            ok = meck:new(ocpp_station_manager),
            meck:expect(
                ocpp_station_manager,
                connect,
                fun(_, Versions) ->
                    case lists:member('2.0.1', Versions) of
                        true -> {ok, '2.0.1'};
                        false -> {error, not_supported}
                    end
                end
            ),
            ok = meck:new(ocpp_timer, [merge_expects]),
            ok = meck:expect(ocpp_timer, cancel, fun(_) -> ok end),
            fun() ->
                meck:unload(ocpp_station_manager),
                meck:unload(ocpp_timer),
                lists:foreach(fun application:stop/1, Apps)
            end
        end,
        ?FORALL(
            Cmds,
            proper_fsm:commands(?MODULE),
            begin
                {ok, _Pid} = ocpp_station:start_link(?STATIONID),
                {History, State, Result} = proper_fsm:run_commands(?MODULE, Cmds),
                Stopped =
                    try
                        ocpp_station:stop(?STATIONID),
                        true
                    catch
                        exit:noproc -> false
                    end,
                ?WHENFAIL(
                    io:format(
                        "History: ~p\nState: ~p\nResult: ~p\n",
                        [History, State, Result]
                    ),
                    aggregate(
                        zip(
                            proper_fsm:state_names(History),
                            command_names(Cmds)
                        ),
                        Result =:= ok andalso Stopped
                    )
                )
            end
        )
    ).

-record(data, {
    %% a call in progress from the station to the csms
    station_cip :: ocpp_rpc:call() | undefined,
    %% list of station-initiated calls that timed out before response from csms
    station_timed_out = [] :: [ocpp_rpc:call()],
    %% a call in progress from the csms to the station
    csms_cip :: {ocpp_rpc:call(), reference()} | undefined,
    %% list of CSMS-initiated calls that timed out before response from station
    csms_timed_out = [] :: [ocpp_rpc:call()],
    %% list of TriggerMessageRequests
    accepted_triggers = [] :: [ocpp_message:message()],
    rejected_triggers = [] :: [ocpp_message:message()]
}).

shuffle(Xs) ->
    [X || {X, _} <- lists:keysort(2, [{X, rand:uniform()} || X <- Xs])].

%% Initial state for the state machine
initial_state() -> offline.

%% Initial model data at the start. Should be deterministic.
initial_state_data() ->
    #data{}.

offline(Data) ->
    general_commands(Data) ++
        [
            {history,
                {call, station201_shim, connect_unsupported, [
                    ?STATIONID, list(oneof(['1.6', '2.1']))
                ]}},
            {
                connected,
                {call, station201_shim, connect_supported, [
                    ?STATIONID,
                    ?LET(Vsns, list(oneof(['1.6', '2.1', '2.0.1'])), shuffle(['2.0.1' | Vsns]))
                ]}
            }
            %% TODO attempt to call ocpp_station:rpc/2 in this state
            %%      attempt to call ocpp_station:reply/3
            %%      attempt to call ocpp_station:disconnect/1
            %%      attempt to call ocpp_station:call/3
            %%      attempt to call ocpp_station:disconnect/1
        ].

connected(Data) ->
    general_commands(Data) ++
        [
            {booting,
                {call, station201_shim, station_call_boot, [
                    ?STATIONID,
                    ?LET(
                        {MessageID, Payload},
                        {
                            messageid(),
                            ocpp_message_gen:message('2.0.1', ~"BootNotificationRequest")
                        },
                        ocpp_rpc:call(Payload, MessageID)
                    )
                ]}},
            station_call_security_error([~"BootNotification"]),
            {history,
                {call, station201_shim, connect_already_connected, [
                    ?STATIONID,
                    ?LET(Vsns, list(oneof(['1.6', '2.1', '2.0.1'])), shuffle(['2.0.1' | Vsns]))
                ]}},
            {offline, {call, station201_shim, station_disconnect, [?STATIONID]}}
        ].

reconnected(Data) ->
    %% booted and accepted, reconnected after going offline without rebooting
    general_commands(Data) ++
        [
            {history,
                {call, ?MODULE, todo, ["allow heartbeat and other messages, allow boot, etc..."]}}
        ].

booting(#data{station_cip = BootCall} = Data) ->
    general_commands(Data) ++
        [
            station_call_security_error([~"BootNotification"]),
            {idle,
                {call, station201_shim, csms_reply_boot_accepted, [
                    ?STATIONID,
                    BootCall,
                    ocpp_message_gen:message(
                        '2.0.1',
                        ~"BootNotificationResponse",
                        [{override, #{status => 'Accepted'}}]
                    )
                ]}},
            {pending,
                {call, station201_shim, csms_reply_boot_pending, [
                    ?STATIONID,
                    BootCall,
                    ocpp_message_gen:message(
                        '2.0.1',
                        ~"BootNotificationResponse",
                        [{override, #{status => 'Pending'}}]
                    )
                ]}},
            {
                connected,
                {call, station201_shim, csms_reply_boot_rejected, [
                    ?STATIONID,
                    BootCall,
                    ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse", [
                        {override, #{status => 'Rejected'}}
                    ])
                ]}
            },
            {offline, {call, station201_shim, station_disconnect, [?STATIONID]}}
            %% TODO allow a subsequent BootNotification here to model the station timing out an retrying.
        ].

pending(Data) ->
    %% received a BootNotificationResponse with status=Pending
    general_commands(Data) ++
        lists:flatten([
            station_call_security_error([~"BootNotification"]),
            config_commands(Data),
            report_commands(Data),
            timedout_response(Data),
            {booting,
                {call, station201_shim, station_call_boot, [
                    ?STATIONID,
                    ?LET(
                        {MessageID, Payload},
                        {
                            messageid(),
                            ocpp_message_gen:message('2.0.1', ~"BootNotificationRequest")
                        },
                        ocpp_rpc:call(Payload, MessageID)
                    )
                ]}}
        ]).

idle(Data) ->
    general_commands(Data) ++
        [
            {
                {idle, cip},
                {call, station201_shim, station_call_heartbeat, [
                    ?STATIONID,
                    ?LET(
                        MessageID,
                        messageid(),
                        begin
                            {ok, Message} = ocpp_message:new('2.0.1', ~"HeartbeatRequest", #{}),
                            ocpp_rpc:call(Message, MessageID)
                        end
                    )
                ]}
            }
        ].

idle(cip, #data{station_cip = PendingCall} = Data) ->
    MessageID = ocpp_rpc:id(PendingCall),
    general_commands(Data) ++
        [
            %% TODO station times out and sends another message
            {idle,
                {call, station201_shim, csms_reply, [
                    ?STATIONID,
                    MessageID,
                    matching_response_payload(PendingCall)
                ]}}
        ].

%% Optional callback, weight modification of transitions
weight(offline, connected, _Call) ->
    20;
weight(connected, booting, _) ->
    20;
weight(booting, pending, _) ->
    20;
weight(booting, idle, _) ->
    2;
weight(
    connected,
    connected,
    {call, station201_shim, station_call_security_error, _}
) ->
    15;
weight(connected, connected, {call, station201_shim, connect_already_connected, _}) ->
    5;
weight(connected, offline, {call, station201_shim, station_disconnect, _}) ->
    5;
weight(pending, pending, _) ->
    %% 3x more likely to stay in the pending state...
    2;
weight(pending, _, _) ->
    %% ... than to leave the pending state
    1;
weight(_FromState, _ToState, _) ->
    1.

precondition(
    _From,
    _To,
    #data{csms_timed_out = TimedOut},
    {call, station201_shim, station_reply_timedout_call, _}
) when length(TimedOut) =:= 0 ->
    false;
%% TODO the following two clauses should be change to true and a test should be added that
%%      in this state a csms_call should fail with an error like {error, not_prvosioned}
precondition(
    _From, _To, #data{csms_cip = CiP}, {call, station201_shim, csms_call, _}
) when CiP =/= undefined ->
    false;
precondition(
    pending, _, #data{csms_cip = CiP}, {call, station201_shim, csms_call, [_, _, Message]}
) ->
    AllowedActions = [~"GetVariables", ~"SetVariables", ~"GetReport", ~"GetBaseReport"],
    CiP =/= undefined andalso not lists:member(ocpp_message:action(Message), AllowedActions);
precondition(
    _From, _To, #data{csms_cip = CiP}, {call, station201_shim, csms_call_get_base_report, _}
) when CiP =/= undefined ->
    false;
precondition(
    _From, _To, #data{csms_cip = CiP}, {call, station201_shim, csms_call_get_report, _}
) when CiP =/= undefined ->
    false;
precondition(
    _From, _To, #data{csms_cip = undefined}, {call, station201_shim, Command, _}
) when
    Command =:= csms_rpccall_timeout;
    Command =:= csms_call_with_cip
->
    false;
precondition(From, _To, _Data, {call, station201_shim, station_reply, _}) when
    From =:= connected;
    From =:= booting
->
    false;
precondition(_From, _To, #data{csms_cip = undefined}, {call, station201_shim, station_reply, _}) ->
    false;
precondition(_From, _To, #data{}, {call, _Mod, _Fun, _Args}) ->
    true.

%% Given the state states and data *prior* to the call
%% `{call, Mod, Fun, Args}', determine if the result `Res' (coming
%% from the actual system) makes sense.
postcondition(
    offline,
    offline,
    _Data,
    {call, station201_shim, connect_unsupported, _},
    {error, not_supported}
) ->
    true;
postcondition(
    offline,
    connected,
    _Data,
    {call, station201_shim, connect_supported, _},
    {ok, '2.0.1'}
) ->
    true;
postcondition(
    _From,
    booting,
    _Data,
    {call, station201_shim, station_call_boot, _},
    ok
) ->
    true;
postcondition(
    From,
    From,
    #data{csms_cip = undefined},
    {call, station201_shim, csms_call, _},
    {error, not_provisioned}
) when
    From =:= connected; From =:= booting
->
    refute_rpcsend();
postcondition(
    From,
    From,
    #data{csms_cip = {RPCCall, _}},
    {call, station201_shim, csms_call, _},
    {error, {call_pending, CallID}}
) when
    From =:= connected; From =:= booting
->
    CallID =:= ocpp_rpc:id(RPCCall) andalso
        refute_rpcsend();
postcondition(
    connected,
    connected,
    _Data,
    {call, station201_shim, connect_already_connected, _},
    {error, already_connected}
) ->
    true;
postcondition(
    connected,
    offline,
    _Data,
    {call, station201_shim, station_disconnect, _},
    ok
) ->
    true;
postcondition(
    booting,
    offline,
    _Data,
    {call, station201_shim, station_disconnect, _},
    ok
) ->
    true;
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, station_call_security_error, [_, RPCCall]},
    ok
) ->
    assert_station_received_callerror(RPCCall, 'SecurityError');
postcondition(
    booting,
    idle,
    _Data,
    {call, station201_shim, csms_reply_boot_accepted, [_, RPCCall, _]},
    ok
) ->
    %% NOTE we don't check the status in the response. These tests are
    %% focused on the overall behavior of the station state machine.
    %% All we want to verify here is that a reply was sent and
    %% received by the connection process (self()) and that following
    %% the reply the station state machine behaves correctly for its
    %% expected state.
    assert_station_received_valid_reply(RPCCall);
postcondition(
    booting,
    pending,
    _Data,
    {call, station201_shim, csms_reply_boot_pending, [_, RPCCall, _]},
    ok
) ->
    assert_station_received_valid_reply(RPCCall);
postcondition(
    booting,
    connected,
    _Data,
    {call, station201_shim, csms_reply_boot_rejected, [_, RPCCall, _]},
    ok
) ->
    assert_station_received_valid_reply(RPCCall);
postcondition(
    idle,
    {idle, cip},
    _Data,
    {call, station201_shim, station_call_heartbeat, _},
    ok
) ->
    true;
postcondition(
    {idle, cip},
    idle,
    #data{station_cip = RPCCall},
    {call, station201_shim, csms_reply, _},
    ok
) ->
    assert_station_received_valid_reply(RPCCall);
postcondition(offline, _, _, {call, station201_shim, csms_call, _}, {error, not_connected}) ->
    refute_rpcsend();
postcondition(
    From,
    _,
    #data{csms_cip = undefined},
    {call, station201_shim, csms_call, _},
    {error, not_provisioned}
) when
    From =:= connected;
    From =:= booting;
    From =:= pending
->
    refute_rpcsend();
postcondition(
    From,
    _,
    #data{csms_cip = {RPCCall, _}},
    {call, station201_shim, csms_call, _},
    {error, {call_pending, ID}}
) when
    From =:= connected;
    From =:= booting;
    From =:= pending
->
    ocpp_rpc:id(RPCCall) =:= ID andalso refute_rpcsend();
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, csms_call, [_, MessageID, _]},
    ok
) ->
    assert_station_received_call(MessageID);
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, csms_call_get_base_report, [_, MessageID, _]},
    ok
) ->
    assert_station_received_call(MessageID);
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, csms_call_get_report, [_, MessageID, _]},
    ok
) ->
    assert_station_received_call(MessageID);
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, StationReplyFun, _},
    ok
) when
    StationReplyFun =:= station_reply;
    StationReplyFun =:= station_reply_report_accepted;
    StationReplyFun =:= station_reply_report_not_accepted
->
    true;
postcondition(_From, _To, _Data, {call, station201_shim, station_reply_timedout_call, _}, ok) ->
    true;
postcondition(
    _From,
    _To,
    #data{csms_cip = {RPCCall, _}},
    {call, station201_shim, csms_call_with_cip, _},
    {error, {call_pending, PendingID}}
) ->
    ocpp_rpc:id(RPCCall) =:= PendingID;
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, csms_rpccall_timeout, _},
    _
) ->
    %% This command just changes the internal state of the station.
    %% There is nothing outwardly visible to validate here.
    true;
postcondition(
    _From,
    _To,
    _Data,
    {call, ?MODULE, todo, _},
    todo
) ->
    true;
postcondition(
    _From,
    _To,
    _Data,
    {call, _Mod, _Fun, _Args} = Call,
    Res
) ->
    io:format("fallthrough: ~p -> ~p\n", [Call, Res]),
    false.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state_data(
    idle, {idle, cip}, Data, _Res, {call, station201_shim, station_call_heartbeat, [_, RPCCall]}
) ->
    Data#data{station_cip = RPCCall};
next_state_data(
    _, booting, Data, _Res, {call, station201_shim, station_call_boot, [_, RPCCall]}
) ->
    Data#data{station_cip = RPCCall};
next_state_data(booting, _, Data, _Res, {call, station201_shim, BootCommand, _}) when
    BootCommand =:= csms_reply_boot_accepted;
    BootCommand =:= csms_reply_boot_pending;
    BootCommand =:= csms_reply_boot_rejected
->
    Data#data{station_cip = undefined};
next_state_data(
    _From,
    _To,
    Data,
    _Res,
    {call, station201_shim, csms_call, [_, MessageID, Request]}
) ->
    Ref = update_timeout_mock(MessageID),
    Data#data{csms_cip = {ocpp_rpc:call(Request, MessageID), Ref}};
next_state_data(
    _From,
    _To,
    Data,
    _Res,
    {call, station201_shim, csms_call_get_base_report, [_, MessageID, Request]}
) ->
    %% TODO once NotifyReportRequest handling is implemented, this
    %% transition will need to handle the follow-on NotifyReport flow
    %% from the station.
    Ref = update_timeout_mock(MessageID),
    Data#data{csms_cip = {ocpp_rpc:call(Request, MessageID), Ref}};
next_state_data(
    _From,
    _To,
    Data,
    _Res,
    {call, station201_shim, csms_call_get_report, [_, MessageID, Request]}
) ->
    %% TODO once NotifyReportRequest handling is implemented, this
    %% transition will need to handle the follow-on NotifyReport flow
    %% from the station.
    Ref = update_timeout_mock(MessageID),
    Data#data{csms_cip = {ocpp_rpc:call(Request, MessageID), Ref}};
next_state_data(
    _From,
    _To,
    Data = #data{csms_cip = {RPCCall, _}},
    _Res,
    {call, station201_shim, csms_rpccall_timeout, [_, RPCCall, _]}
) ->
    Data#data{csms_cip = undefined, csms_timed_out = [RPCCall | Data#data.csms_timed_out]};
next_state_data(_From, _To, Data, _Res, {call, station201_shim, StationReplyFun, [_, _, _]}) when
    StationReplyFun =:= station_reply;
    StationReplyFun =:= station_reply_report_accepted;
    StationReplyFun =:= station_reply_report_not_accepted
->
    Data#data{csms_cip = undefined};
next_state_data(
    connected,
    offline,
    Data,
    _Res,
    {call, station201_shim, station_disconnect, _}
) ->
    %% TODO Should csms_cip be cleared here? (and below?) what about pending->offline?
    Data#data{station_cip = undefined, csms_cip = undefined};
next_state_data(
    booting,
    offline,
    Data,
    _Res,
    {call, station201_shim, station_disconnect, _}
) ->
    Data#data{station_cip = undefined, csms_cip = undefined};
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    NewData = Data,
    NewData.

%% Command generation helper functinos
timedout_response(#data{csms_timed_out = []}) ->
    [];
timedout_response(#data{csms_timed_out = TimedOut}) ->
    [
        {history,
            {call, station201_shim, station_reply_timedout_call, [
                ?STATIONID,
                oneof([
                    {Call, matching_response_payload(Call)}
                 || Call <- TimedOut
                ])
            ]}}
    ].

csms_call_pending_command(#data{csms_cip = undefined}) ->
    [];
csms_call_pending_command(#data{csms_cip = {RPCCall, TimerRef}}) ->
    [
        {history,
            {call, station201_shim, csms_call_with_cip, [
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1')
            ]}},
        {history, {call, station201_shim, csms_rpccall_timeout, [?STATIONID, RPCCall, TimerRef]}}
    ].

%% This defines a command to test B01.FR.10, B02.FR.05, B03.FR.08
station_call_security_error(AllowedActions) ->
    {history,
        {call, station201_shim, station_call_security_error, [
            ?STATIONID,
            ?LET(
                Payload,
                ?SUCHTHAT(
                    Message,
                    ocpp_message_gen:request('2.0.1'),
                    lists:all(
                        fun(Action) -> ocpp_message:action(Message) =/= Action end, AllowedActions
                    )
                ),
                ocpp_rpc:call(Payload, messageid())
            )
        ]}}.

matching_response_payload(RPCCall) ->
    Action = ocpp_rpc:action(RPCCall),
    matching_response_payload(RPCCall, Action).

%% Construct a response payload that matches the variable data in the request.
matching_response_payload(RPCCall, ~"SetVariables") ->
    Message = ocpp_rpc:payload(RPCCall),
    VarData = ocpp_message:get('setVariableData', Message),
    ocpp_message_gen:message('2.0.1', ~"SetVariablesResponse", [
        {override, #{
            setVariableResult => [
                maps:with([component, variable, attributeType], V)
             || V <- VarData
            ]
        }}
    ]);
matching_response_payload(RPCCall, ~"GetVariables") ->
    Message = ocpp_rpc:payload(RPCCall),
    VarData = ocpp_message:get('getVariableData', Message),
    ocpp_message_gen:message('2.0.1', ~"GetVariablesResponse", [
        {override, #{
            getVariableResult => [
                maps:with([component, variable, attributeType], V)
             || V <- VarData
            ]
        }}
    ]);
matching_response_payload(_RPCCall, ~"GetBaseReport") ->
    ocpp_message_gen:message('2.0.1', ~"GetBaseReportResponse", [
        %% {override, #{status => 'Rejected'}}
    ]);
matching_response_payload(_RPCCall, ~"GetReport") ->
    ocpp_message_gen:message('2.0.1', ~"GetReportResponse", [
        %% {override, #{status => 'Rejected'}}
    ]);
matching_response_payload(_RPCCall, ~"TriggerMessage") ->
    ocpp_message_gen:message('2.0.1', ~"TriggerMessageResponse", [
        %% {override, #{status => 'Rejected'}}
    ]);
matching_response_payload(_RPCCall, Action) ->
    ocpp_message_gen:message('2.0.1', <<Action/binary, "Response">>).

%% CSMS-initiated commands (B05 SetVariables, B06 GetVariables, TriggerMessage).
%% Uses oneof to let proper pick which message type to send, avoiding a
%% duplicate {Mod, Fun, Arity} transition error from proper_fsm.
config_commands(#data{csms_cip = undefined}) ->
    csms_config_call();
config_commands(_) ->
    [].

csms_config_call() ->
    Request = oneof([
        ocpp_message_gen:message('2.0.1', ~"SetVariablesRequest"),
        ocpp_message_gen:message('2.0.1', ~"GetVariablesRequest")
    ]),
    [
        {history, {call, station201_shim, csms_call, [?STATIONID, messageid(), Request]}}
    ].

%% CSMS-initiated report commands (B07 GetBaseReport, B08 GetReport).
%% Response includes report_responses, gated on csms_cip in model data.
report_commands(Data) ->
    [
        {history,
            {call, station201_shim, csms_call_get_base_report, [
                ?STATIONID,
                messageid(),
                ocpp_message_gen:message('2.0.1', ~"GetBaseReportRequest")
            ]}},
        {history,
            {call, station201_shim, csms_call_get_report, [
                ?STATIONID,
                messageid(),
                ocpp_message_gen:message('2.0.1', ~"GetReportRequest")
            ]}}
    ] ++ report_responses(Data).

report_responses(#data{csms_cip = {RPCCall, _}}) ->
    case ocpp_rpc:action(RPCCall) of
        Action when Action =:= ~"GetBaseReport"; Action =:= ~"GetReport" ->
            AcceptedPayload =
                ocpp_message_gen:message(
                    '2.0.1',
                    <<Action/binary, "Response">>,
                    [{override, #{status => 'Accepted'}}]
                ),
            NotAcceptedPayload =
                ocpp_message_gen:message(
                    '2.0.1',
                    <<Action/binary, "Response">>,
                    [{override, #{status => oneof(['Rejected', 'NotSupported', 'EmptyResultSet'])}}]
                ),
            [
                {history,
                    {call, station201_shim, station_reply_report_accepted, [
                        ?STATIONID, RPCCall, AcceptedPayload
                    ]}},
                {history,
                    {call, station201_shim, station_reply_report_not_accepted, [
                        ?STATIONID, RPCCall, NotAcceptedPayload
                    ]}}
            ];
        _ ->
            []
    end;
report_responses(_) ->
    [].

station_generic_reply(#data{csms_cip = undefined}) ->
    [];
station_generic_reply(#data{csms_cip = {RPCCall, _}}) ->
    [
        {history,
            {call, station201_shim, station_reply, [
                ?STATIONID, RPCCall, matching_response_payload(RPCCall)
            ]}}
    ].

general_commands(Data) ->
    csms_call_pending_command(Data) ++ station_generic_reply(Data).

todo(What) ->
    io:format("WARNING - TODO (~s)\n", [What]),
    todo.

%% Helper to generate unique message IDs
update_timeout_mock(MessageID) ->
    Ref = erlang:make_ref(),
    ok = meck:expect(ocpp_timer, set_timeout, ['_', {rpccall, MessageID}], {ok, Ref}),
    Ref.

messageid() ->
    ?LAZY(integer_to_binary(erlang:unique_integer([positive]), 36)).

assert_station_received_valid_reply(RPCCall) ->
    assert_station_received_valid_reply(RPCCall, fun(_) -> true end).

assert_station_received_valid_reply(RPCCall, ValidationFun) ->
    receive
        {ocpp, {rpcsend, ReplyBin}} ->
            Action = ocpp_message:action(ocpp_rpc:payload(RPCCall)),
            case ocpp_rpc:decode('2.0.1', ReplyBin, [{expected, Action}]) of
                {ok, {callresult, RPCResult}} ->
                    ocpp_rpc:id(RPCResult) =:= ocpp_rpc:id(RPCCall) andalso
                        ValidationFun(RPCResult);
                _ ->
                    false
            end
    after 50 ->
        false
    end.

assert_station_received_callerror(RPCCall, ErrorCode) ->
    receive
        {ocpp, {rpcsend, ReplyBin}} ->
            Action = ocpp_message:action(ocpp_rpc:payload(RPCCall)),
            case ocpp_rpc:decode('2.0.1', ReplyBin, [{expected, Action}]) of
                {ok, {callerror, Error}} ->
                    ocpp_rpc:error_code(Error) =:= ErrorCode andalso
                        ocpp_rpc:id(Error) =:= ocpp_rpc:id(RPCCall);
                _ ->
                    false
            end
    after 100 ->
        false
    end.

refute_rpcsend() ->
    receive
        {ocpp, {rpcsend, _}} ->
            false
    after 50 ->
        true
    end.

assert_station_received_call(MessageID) ->
    receive
        {ocpp, {rpcsend, EncodedMessage}} ->
            case ocpp_rpc:decode('2.0.1', EncodedMessage, []) of
                {ok, {call, RPCCall}} ->
                    ocpp_rpc:id(RPCCall) =:= MessageID;
                _ ->
                    false
            end
    after 50 ->
        false
    end.
