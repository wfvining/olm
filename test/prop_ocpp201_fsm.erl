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
    offline/2,
    connected/2,
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
initial_state() -> {offline, before_boot}.
%% Initial model data at the start. Should be deterministic.
initial_state_data() ->
    #data{}.

offline(Booted, _Data) ->
    [
        {history,
            {call, station201_shim, connect_unsupported, [?STATIONID, list(oneof(['1.6', '2.1']))]}},
        {
            {connected, Booted},
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

connected(before_boot, _Data) ->
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
        {history,
            {call, station201_shim, station_call_before_boot, [
                ?STATIONID,
                ?SUCHTHAT(
                    Message,
                    ocpp_message_gen:request('2.0.1'),
                    ocpp_message:action(Message) =/= ~"BootNotification"
                )
            ]}},
        {history,
            {call, station201_shim, csms_call_before_boot, [
                ?STATIONID,
                ocpp_message_gen:request('2.0.1')
            ]}}
    ];
connected(accepted, _Data) ->
    %% booted and accepted
    [{history, {call, ?MODULE, todo, ["allow heartbeat and other messages, allow boot, etc..."]}}];
connected(after_boot, _Data) ->
    %% booted but not accepted
    [
        functional_block_b_security_error(),
        {history,
            {call, station201_shim, csms_call_after_boot, [
                ?STATIONID, ocpp_message_gen:request('2.0.1'), messageid()
            ]}},
        {booting,
            {call, station201_shim, station_call_boot, [
                ?STATIONID,
                ?LET(
                    {BootRequest, MessageID},
                    {ocpp_message_gen:message('2.0.1', ~"BootNotificationRequest"), messageid()},
                    ocpp_rpc:call(BootRequest, MessageID)
                )
            ]}}
    ].

booting(#data{station_cip = BootCall}) ->
    [
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
        {
            {connected, before_boot},
            {call, station201_shim, station_call_before_boot, [
                ?STATIONID,
                ?SUCHTHAT(
                    Message,
                    ocpp_message_gen:request('2.0.1'),
                    ocpp_message:action(Message) =/= ~"BootNotification"
                )
            ]}
        },
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
            {connected, after_boot},
            {call, station201_shim, csms_reply_boot_rejected, [
                ?STATIONID,
                BootCall,
                ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse", [
                    {override, #{status => 'Rejected'}}
                ])
            ]}
        }
    ].

pending(Data) ->
    %% received a BootNotificationResponse with status=Pending
    %, use_case_B06(), use_case_B07(), use_case_B08()]).
    lists:flatten([
        %% functional_block_b_security_error(),
        use_case_B05(Data),
        csms_call_pending_command(Data),
        timedout_response(Data)
    ]).

timedout_response(#data{csms_timed_out = []}) ->
    [];
timedout_response(#data{csms_timed_out = TimedOut}) ->
    [
        {history,
            {call, station201_shim, station_reply_timedout_call, [
                ?STATIONID,
                oneof([
                    {Call, generic_reply(Call)}
                 || Call <- TimedOut
                ])
            ]}}
    ].

csms_call_pending_command(#data{csms_cip = undefined}) ->
    [];
csms_call_pending_command(#data{csms_cip = {RPCCall, TimerRef}}) ->
    %% {history, {call, station201_shim, csms_call_with_cip,
    [
        %%            [?STATIONID, ocpp_message_gen:request('2.0.1')]}},
        {history, {call, station201_shim, csms_rpccall_timeout, [?STATIONID, RPCCall, TimerRef]}}
    ].

use_case_B05(Data) ->
    %% SetVariables
    [
        {history,
            {call, station201_shim, csms_call_set_variables, [
                ?STATIONID, messageid(), ocpp_message_gen:message('2.0.1', ~"SetVariablesRequest")
            ]}}
        | use_case_B05_responses(Data)
    ].

use_case_B05_responses(#data{csms_cip = undefined}) ->
    [];
use_case_B05_responses(#data{csms_cip = {RPCCall, _}}) ->
    case ocpp_rpc:action(RPCCall) of
        ~"SetVariables" ->
            [
                {history,
                    {call, station201_shim, station_reply_set_variables, [
                        ?STATIONID,
                        RPCCall,
                        set_variables_response_payload(RPCCall)
                    ]}}
            ];
        _ ->
            []
    end.

set_variables_response_payload(Call) ->
    Message = ocpp_rpc:payload(Call),
    VarData = ocpp_message:get('setVariableData', Message),
    ocpp_message_gen:message(
        '2.0.1',
        ~"SetVariablesResponse",
        [
            {override, #{
                setVariableResult => [
                    maps:with([component, variable, attributeType], Var)
                 || Var <- VarData
                ]
            }}
        ]
    ).

%% This defines a command to test B01.FR.10, B02.FR.05, B03.FR.08
functional_block_b_security_error() ->
    {history,
        {call, station201_shim, station_call_security_error, [
            ?STATIONID,
            ?LET(
                Payload,
                ?SUCHTHAT(
                    Message,
                    ocpp_message_gen:request('2.0.1'),
                    ocpp_message:action(Message) =/= ~"BootNotification"
                ),
                ocpp_rpc:call(Payload, messageid())
            )
        ]}}.

todo(What) ->
    io:format("WARNING - TODO (~s)\n", [What]),
    todo.

idle(_Data) ->
    %% Station has booted and is ready for normal operation
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

idle(cip, #data{station_cip = PendingCall}) ->
    MessageID = ocpp_rpc:id(PendingCall),
    [
        {idle,
            {call, station201_shim, csms_reply, [
                ?STATIONID,
                MessageID,
                generic_reply(PendingCall)
            ]}}
    ].

generic_reply(RPCCall) ->
    Action = ocpp_message:action(ocpp_rpc:payload(RPCCall)),
    ocpp_message_gen:message('2.0.1', <<Action/binary, "Response">>).

%% Optional callback, weight modification of transitions
weight({offline, _}, {connected, _}, _Call) -> 100;
weight({connected, _}, booting, _) -> 100;
weight(booting, pending, _) -> 100;
weight(booting, idle, _) -> 2;
weight(booting, {connected, after_boot}, _) -> 5;
weight(pending, pending, {call, station201_shim, station_reply_set_variables_expired, _}) -> 10;
weight(_FromState, _ToState, {_, _, Fun, _}) -> 1.

precondition(
    _From,
    _To,
    #data{csms_timed_out = TimedOut},
    {call, station201_shim, station_reply_timedout_call, _}
) when length(TimedOut) =:= 0 ->
    false;
precondition(
    {connected, SubState}, _To, _Data, {call, station201_shim, station_call_security_error, _}
) when
    SubState =/= before_boot
->
    false;
precondition(From, _To, _Data, {call, station201_shim, station_call_security_error, _}) when
    From =/= pending
->
    false;
precondition(
    {connected, SubState}, _To, _Data, {call, station201_shim, csms_call_set_variables, _}
) when
    SubState =/= accepted
->
    false;
precondition({offline, _}, _To, _Data, {call, station201_shim, csms_call_set_variables, _}) ->
    false;
precondition(booting, _To, _Data, {call, station201_shim, csms_call_set_variables, _}) ->
    false;
precondition(
    _From, _To, #data{csms_cip = CiP}, {call, station201_shim, csms_call_set_variables, _}
) when CiP =/= undefined ->
    false;
precondition(
    _From, _To, #data{csms_cip = undefined}, {call, station201_shim, station_reply_set_variables, _}
) ->
    false;
precondition(
    _From, _To, #data{csms_cip = undefined}, {call, station201_shim, Command, _}
) when
    Command =:= csms_rpccall_timeout;
    Command =:= csms_call_with_cip
->
    false;
precondition(_From, _To, #data{}, {call, _Mod, _Fun, _Args}) ->
    true.

%% Given the state states and data *prior* to the call
%% `{call, Mod, Fun, Args}', determine if the result `Res' (coming
%% from the actual system) makes sense.
postcondition(
    {offline, _},
    {offline, _},
    _Data,
    {call, station201_shim, connect_unsupported, _},
    {error, not_supported}
) ->
    true;
postcondition(
    {offline, _},
    {connected, _},
    _Data,
    {call, station201_shim, connect_supported, _},
    {ok, '2.0.1'}
) ->
    true;
postcondition(
    {connected, _},
    booting,
    _Data,
    {call, station201_shim, station_call_boot, _},
    ok
) ->
    true;
postcondition(
    _From,
    {connected, before_boot},
    _Data,
    {call, station201_shim, station_call_before_boot, _},
    ok
) ->
    refute_rpcsend();
postcondition(
    From,
    From,
    _Data,
    {call, station201_shim, csms_call_before_boot, _},
    {error, not_provisioned}
) when
    From =:= {connected, before_boot}; From =:= booting
->
    refute_rpcsend();
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, station_call_security_error, [_, RPCCall]},
    ok
) ->
    assert_station_received_callerror(RPCCall);
postcondition(
    {connected, after_boot},
    {connected, after_boot},
    _Data,
    {call, station201_shim, csms_call_after_boot, _},
    {error, not_provisioned}
) ->
    refute_rpcsend();
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
    {connected, after_boot},
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
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, csms_call_set_variables, [_, MessageID, _]},
    ok
) ->
    assert_station_received_call(MessageID);
postcondition(
    _From,
    _To,
    _Data,
    {call, station201_shim, station_reply_set_variables, _},
    ok
) ->
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
    {connected, _}, booting, Data, _Res, {call, station201_shim, station_call_boot, [_, RPCCall]}
) ->
    Data#data{station_cip = RPCCall};
next_state_data(
    booting,
    {connected, before_boot},
    Data,
    _Res,
    {call, station201_shim, station_call_before_boot, _}
) ->
    Data#data{station_cip = undefined};
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
    {call, station201_shim, csms_call_set_variables, [_, MessageID, Request]}
) ->
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
next_state_data(_From, _To, Data, _Res, {call, station201_shim, station_reply_set_variables, _}) ->
    Data#data{csms_cip = undefined};
next_state_data(
    _From,
    _To,
    Data,
    _Res,
    {call, station201_shim, station_reply_set_variables_expired, [_, RPCCall, _]}
) ->
    Data#data{csms_timed_out = lists:delete(RPCCall, Data#data.csms_timed_out)};
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    NewData = Data,
    NewData.

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

assert_station_received_callerror(RPCCall) ->
    receive
        {ocpp, {rpcsend, ReplyBin}} ->
            Action = ocpp_message:action(ocpp_rpc:payload(RPCCall)),
            case ocpp_rpc:decode('2.0.1', ReplyBin, [{expected, Action}]) of
                {ok, {callerror, Error}} ->
                    ocpp_rpc:error_code(Error) =:= 'SecurityError' andalso
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
