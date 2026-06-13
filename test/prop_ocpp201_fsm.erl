-module(prop_ocpp201_fsm).
%% @docfile "OCPP-2.0.1-FSM.md"
%%
%% The following causes some kind of problem with epp, possibly related to the proper
%% parse-transforms
%% -moduledoc {file, "OCPP-2.0.1-FSM.md"}.
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
-export([unprovisioned/3, booting/3, pending/3, accepted/3]).

-define(STATIONID, atom_to_binary(?MODULE)).
-define(CALL_CONNECT_UNSUPPORTED,
    {call, station201_shim, connect_unsupported, [?STATIONID, version_list(unsupported)]}
).
-define(CALL_CONNECT_SUPPORTED,
    {call, station201_shim, connect_supported, [?STATIONID, version_list(supported)]}
).

-record(data, {
    %% call in progress from the station to the csms. only set for
    %% calls that reach the CSMS and are "accepted" for processing
    %% (i.e. calls that can receive a response)
    station_cip :: ocpp_rpc:call() | undefined,
    csms_cip :: {ocpp_rpc:call(), reference()} | undefined,
    csms_timedout :: ocpp_rpc:call() | undefined,
    triggers = {[], []} :: {ocpp_message:message(), ocpp_message:message()},
    report_requests = [] :: [{ocpp_message:message(), integer()}]
}).

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
                        [zip(History, Cmds), State, Result]
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

%% Initial state for the state machine
initial_state() -> {unprovisioned, disconnected, {up, clean}}.

%% Initial model data at the start. Should be deterministic.
initial_state_data() ->
    #data{}.

unprovisioned(disconnected, {up, _} = StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, disconnected, StationState) ++
        csms_call_reply(unprovisioned, disconnected, StationState, Data) ++
        station_call_reply(unprovisioned, disconnected, StationState, Data);
unprovisioned(connected, {up, _} = StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, connected, StationState) ++
        csms_call_reply(unprovisioned, connected, StationState, Data) ++
        station_call_reply(unprovisioned, connected, StationState, Data).

booting(connected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, connected, StationState) ++
        csms_call_reply(booting, connected, StationState, Data) ++
        station_call_reply(booting, connected, StationState, Data);
booting(disconnected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, disconnected, StationState) ++
        csms_call_reply(booting, disconnected, StationState, Data) ++
        station_call_reply(booting, disconnected, StationState, Data).

pending(connected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, connected, StationState) ++
        csms_call_reply(pending, connected, StationState, Data) ++
        station_call_reply(pending, connected, StationState, Data);
pending(disconnected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, disconnected, StationState) ++
        csms_call_reply(pending, disconnected, StationState, Data) ++
        station_call_reply(pending, disconnected, StationState, Data).

accepted(connected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, connected, StationState);
%% csms_call_reply(accepted, ConnectionState, StationState, Data) ++
%%     station_call_reply(accepted, ConnectionState, StationState, Data) ++

accepted(disconnected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, disconnected, StationState).

precondition(_, _, _, {call, station201_shim, csms_rpccall_timeout, _}) ->
    true;
precondition(
    {AppState, ConnFrom, {up, Flag}},
    {AppState, ConnTo, {up, Flag}},
    _Data,
    {call, _, Fun, _}
) when
    ConnFrom =:= disconnected, ConnTo =:= connected, Fun =:= connect_supported;
    ConnFrom =:= connected, ConnTo =:= disconnected, Fun =:= station_disconnect
->
    true;
precondition(
    {_, disconnected, _},
    {_, disconnected, _},
    _Data,
    {call, _, station_power_cycle}
) ->
    true;
precondition(
    {_, connected, _},
    {booting, connected, {up, clean}},
    _Data,
    {call, _, station_call, [_, RPCCall]}
) ->
    ocpp_rpc:action(RPCCall) =:= ~"BootNotification";
precondition(
    {booting, connected, _},
    {NextState, connected, _},
    #data{station_cip = CiP},
    {call, _, csms_reply, [_, MessageID, Payload]}
) when
    NextState =/= booting,
    CiP =/= undefined
->
    %% all other csms_reply calls in `booting` do not change the state (covered below)
    Status = ocpp_message:get(status, Payload, undefined),
    Res =
        MessageID =:= ocpp_rpc:id(CiP) andalso
            ~"BootNotification" =:= ocpp_message:action(Payload) andalso
            ((NextState =:= accepted andalso 'Accepted' =:= Status) orelse
                (NextState =:= pending andalso 'Pending' =:= Status) orelse
                (NextState =:= unprovisioned andalso 'Rejected' =:= Status)),
    Res;
precondition(
    {booting, connected, _} = From,
    From,
    #data{station_cip = CiP},
    {call, _, csms_reply, [_, MessageID, Payload]}
) ->
    %% Can only do csms_reply and stay in the same state if the reply is not valid
    MessageID =/= ocpp_rpc:id(CiP) orelse ~"BootNotification" =/= ocpp_message:action(Payload);
precondition(
    {booting, connected, _},
    {unprovisioned, connected, _},
    _Data,
    {call, _, station_call_security_error, [_, RPCCall]}
) ->
    %% any message other than a BootNotification MUST change the state to unprovisioned
    ocpp_rpc:action(RPCCall) =/= ~"BootNotification";
precondition(From, From, _Data, {call, _, Fun, [_, RPCCall]}) when
    Fun =:= station_call; Fun =:= station_call_security_error
->
    ocpp_rpc:action(RPCCall) =/= ~"BootNotification";
precondition(
    {AppState, connected, _} = From,
    From,
    #data{triggers = {Accepted, _}},
    {call, _, station_call_triggered, [_, RPCCall]}
) when AppState =:= pending; AppState =:= accepted ->
    %% XXX The condition here needs to be refined to handle specific trigger instances (e.g. evse/connector)
    lists:member(ocpp_rpc:action(RPCCall), [
        Action
     || {Action, _} <- Accepted
    ]);
precondition({_, connected, _} = From, From, Data, {call, _, csms_reply_badid, [_, MessageID, _]}) ->
    Data#data.station_cip =:= undefined orelse ocpp_rpc:id(Data#data.station_cip) =/= MessageID;
precondition(
    {_, connected, _} = From, From, Data, {call, _, csms_reply_badaction, [_, MessageID, Payload]}
) ->
    Action = ocpp_message:action(Payload),
    Data#data.station_cip =/= unedfined andalso ocpp_rpc:action(Data#data.station_cip) =/= Action andalso
        ocpp_rpc:id(Data#data.station_cip) =:= MessageID;
precondition({_, ConnState, _} = From, From, _Data, {call, _, Fun, _}) when
    Fun =:= csms_call;
    Fun =:= csms_reply;
    Fun =:= station_reply;
    Fun =:= station_reply_badaction;
    Fun =:= station_reply_badid;
    Fun =:= station_reply_timedout;
    Fun =:= connect_unsupported;
    Fun =:= connect_supported, ConnState =:= connected
->
    true;
precondition(_From, _To, _Data, _Call) ->
    false.

weight({unprovisioned, _, _}, {booting, _, _}, _) ->
    5;
weight({_, disconnected, _}, {_, connected, _}, _) ->
    20;
weight({booting, connected, _}, {pending, connected, _}, _) ->
    10;
weight({booting, connected, _}, {accepted, connected, _}, _) ->
    1;
weight({pending, connected, _}, _, {call, _, csms_call, _}) ->
    15;
weight({pending, connected, _}, _, {call, _, station_reply, _}) ->
    30;
weight({pending, connected, _}, _, {call, _, station_call_triggered, _}) ->
    120;
weight({pending, connected, _}, _, {call, _, csms_reply, _}) ->
    60;
weight({pending, connected, _}, _, {call, _, Fun, _}) when
    Fun =:= csms_reply_badid; Fun =:= csms_reply_badaction
->
    30;
weight({pending, connected, _}, _, {call, _, Fun, _}) when
    Fun =:= connect_supported; Fun =:= connect_unsupported
->
    1;
weight({pending, connected, _}, _, {call, _, station_reply_badid, _}) ->
    5;
weight({pending, connected, _}, {pending, connected, _}, _) ->
    15;
weight(_From, _To, _Call) ->
    1.

next_state_data(
    _, _, #data{csms_cip = {RPCCall, TRef}} = Data, _, {call, _, csms_rpccall_timeout, [_, _, TRef]}
) ->
    Data#data{csms_cip = undefined, csms_timedout = RPCCall};
next_state_data(
    {_, connected, {up, _}},
    {booting, connected, {up, clean}},
    Data,
    _Result,
    {call, _, station_call, [_, RPCCall]}
) ->
    Data#data{station_cip = RPCCall, csms_cip = undefined};
next_state_data(
    From,
    From,
    #data{triggers = {Accepted, Rejected}} = Data,
    _Result,
    {call, _, station_call_triggered, [_, RPCCall]}
) ->
    Data#data{triggers = {remove_trigger(RPCCall, Accepted), Rejected}, station_cip = RPCCall};
next_state_data(
    {booting, connected, _}, {NextState, connected, _}, Data, _Result, _
) when NextState =/= booting ->
    %% if we are leaving the booting state then the call in progress is compeleted
    Data#data{station_cip = undefined};
next_state_data(
    {_, disconnected, _},
    {_, disconnected, _},
    Data,
    _Result,
    {call, _, station_power_cycle, _}
) ->
    %% TODO clear pending reports and triggers
    Data#data{station_cip = undefined};
next_state_data(
    {pending, connected, _} = From,
    From,
    Data = #data{csms_cip = undefined},
    _Result,
    {call, _, csms_call, [_, MessageID, Message, Ref]}
) ->
    ConfigMessages = [
        ~"SetVariablesRequest",
        ~"GetVariablesRequest",
        ~"GetReportRequest",
        ~"GetBaseReportRequest",
        ~"TriggerMessageRequest"
    ],
    case lists:member(ocpp_message:type(Message), ConfigMessages) of
        true ->
            ok = meck:expect(ocpp_timer, set_timeout, ['_', {rpccall, MessageID}], {ok, Ref}),
            RPC = ocpp_rpc:call(Message, MessageID),
            Data#data{csms_cip = {RPC, Ref}};
        false ->
            Data
    end;
next_state_data(
    {AppState, connected, _} = From,
    From,
    Data = #data{csms_cip = {RPCCall, _TRef}},
    _Result,
    {call, _, station_reply, [_, RPCCall, Reply]}
) when AppState =:= accepted; AppState =:= pending ->
    update_triggers(
        Data#data{csms_cip = undefined, csms_timedout = undefined}, RPCCall, Reply
    );
next_state_data(
    {_, connected, _} = From, From, Data, _Result, {call, _, station_reply_badaction, _}
) ->
    Data#data{csms_cip = undefined};
next_state_data(
    {pending, connected, _} = From,
    From,
    Data = #data{station_cip = CiP},
    _Result,
    {call, _, csms_reply, [_, MessageID, Message]}
) when CiP =/= undefined ->
    PendingID = ocpp_rpc:id(CiP),
    PendingAction = ocpp_rpc:action(CiP),
    ResponseAction = ocpp_message:action(Message),
    if
        PendingID =:= MessageID,
        PendingAction =:= ResponseAction ->
            Data#data{station_cip = undefined};
        true ->
            Data
    end;
next_state_data(_From, _To, Data, _Result, _Call) ->
    Data.

remove_trigger(_, []) ->
    [];
remove_trigger(RPCCall, [Trigger | Rest]) ->
    Action = ocpp_rpc:action(RPCCall),
    case Trigger of
        {Action, _} ->
            Rest;
        _ ->
            [Trigger | remove_trigger(RPCCall, Rest)]
    end.

update_triggers(
    Data = #data{triggers = {AcceptedTriggers, RejectedTriggers}, report_requests = ReportRequests},
    RPCCall,
    Reply
) ->
    Call = ocpp_rpc:payload(RPCCall),
    case ?debugVal(ocpp_rpc:action(RPCCall)) of
        ~"TriggerMessage" ->
            Requested = atom_to_binary(ocpp_message:get(requestedMessage, Call)),
            EVSE = ocpp_message:get(evse, Call, undefined),
            case ocpp_message:get(status, Reply) of
                'Accepted' ->
                    Data#data{
                        triggers = {[{Requested, EVSE} | AcceptedTriggers], RejectedTriggers}
                    };
                'Rejected' ->
                    Data#data{
                        triggers = {AcceptedTriggers, [{Requested, EVSE} | RejectedTriggers]}
                    };
                _Status ->
                    Data
            end;
        Action when Action =:= ~"GetReport"; Action =:= ~"GetBaseReport" ->
            case ocpp_message:get(status, Reply) of
                'Accepted' ->
                    Data#data{report_requests = [ocpp_rpc:payload(RPCCall) | ReportRequests]};
                _ ->
                    Data
            end;
        _ ->
            Data
    end.

postcondition(_, _, _, {call, _, csms_rpccall_timeout, _}, ok) ->
    true;
postcondition({_, _, {up, _}}, {_, _, {up, _}}, _Data, {call, _, station_power_cycle, _}, ok) ->
    true;
postcondition(
    {_, connected, _}, {_, disconnected, _}, _Data, {call, _, station_disconnect, _}, ok
) ->
    true;
postcondition(
    {_, disconnected, _}, {_, connected, _}, _Data, {call, _, connect_supported, _}, {ok, '2.0.1'}
) ->
    true;
postcondition(
    {_, disconnected, _} = From,
    From,
    _Data,
    {call, _, connect_unsupported, _},
    {error, not_supported}
) ->
    true;
postcondition(
    {_, connected, _} = From, From, _Data, {call, _, Fun, _}, {error, already_connected}
) when
    Fun =:= connect_supported;
    Fun =:= connect_unsupported
->
    true;
postcondition(
    {_, disconnected, _} = From, From, _Data, {call, _, Fun, _}, {error, not_connected}
) when
    Fun =:= csms_call; Fun =:= csms_reply
->
    refute_rpcsend();
postcondition(
    {AppState, connected, _} = From,
    From,
    Data,
    {call, _, csms_reply, [_, MessageID, _]},
    {error, {call_not_pending, MessageID}}
) when AppState =/= unprovisioned ->
    (Data#data.station_cip =:= undefined orelse MessageID =/= ocpp_rpc:id(Data#data.station_cip)) andalso
        refute_rpcsend();
postcondition(
    {_, connected, _} = From,
    From,
    #data{station_cip = CiP},
    {call, _, csms_reply, [_, MessageID, Payload]},
    {error, badaction}
) when
    CiP =/= undefined
->
    MessageID =:= ocpp_rpc:id(CiP) andalso
        ocpp_rpc:action(CiP) =/= ocpp_message:action(Payload) andalso
        refute_rpcsend();
postcondition(
    {_, disconnected, _} = From, From, _Data, {call, _, Fun, _}, {error, not_connected}
) when
    Fun =:= station_call;
    Fun =:= station_reply;
    Fun =:= station_reply_badaction;
    Fun =:= station_reply_badid;
    Fun =:= station_reply_timedout
->
    true;
postcondition({_, connected, _} = From, From, _Data, {call, _, Fun, _}, ok) when
    Fun =:= station_reply;
    Fun =:= station_reply_badaction;
    Fun =:= station_reply_badid;
    Fun =:= station_reply_timedout
->
    true;
postcondition(
    {_, connected, _} = From,
    {booting, connected, _} = To,
    Data,
    {call, _, station_call, [_, RPCCall]} = Call,
    ok
) ->
    case ocpp_rpc:action(RPCCall) of
        ~"BootNotification" ->
            true;
        _ ->
            postcondition_dispatch(From, To, Data, Call, ok)
    end;
postcondition(From, To, Data, Call, Result) ->
    postcondition_dispatch(From, To, Data, Call, Result).

postcondition_dispatch({AppState, _ConnectionState, _StationState} = From, To, Data, Call, Result) ->
    case AppState of
        unprovisioned ->
            postcondition_unprovisioned(From, To, Data, Call, Result);
        booting ->
            postcondition_booting(From, To, Data, Call, Result);
        pending ->
            postcondition_pending(From, To, Data, Call, Result);
        accepted ->
            postcondition_accepted(From, To, Data, Call, Result)
    end.

postcondition_unprovisioned(
    {_, connected, _} = From, From, _Data, {call, _, Fun, _}, {error, not_provisioned}
) when
    Fun =:= csms_call; Fun =:= csms_reply
->
    refute_rpcsend();
postcondition_unprovisioned(
    {_, connected, _} = From, From, _Data, {call, _, station_call_security_error, [_, RPCCall]}, ok
) ->
    %% Any CALL that does not change state causes a SecurityError when unprovisioned
    assert_callerror(RPCCall, 'SecurityError');
postcondition_unprovisioned(From, To, Data, Call, Result) ->
    ?debugFmt(
        "[postcondition:unprovisioned] ~p -> ~p via ~p. Result = ~p",
        [From, To, Call, Result]
    ),
    false.

postcondition_booting(
    {_, connected, _} = From, From, _Data, {call, _, csms_call, _}, {error, not_provisioned}
) ->
    refute_rpcsend();
postcondition_booting(
    {_, connected, _},
    {unprovisioned, connected, _},
    _Data,
    {call, _, station_call_security_error, [_, RPCCall]},
    ok
) ->
    assert_callerror(RPCCall, 'SecurityError');
postcondition_booting(
    {_, connected, _}, {NextState, connected, _}, Data, {call, _, csms_reply, [_, _, Payload]}, ok
) when NextState =/= booting ->
    Status = ocpp_message:get(status, Payload, undefined),
    ~"BootNotification" =:= ocpp_message:action(Payload) andalso
        ((accepted =:= NextState andalso 'Accepted' =:= Status) orelse
            (pending =:= NextState andalso 'Pending' =:= Status) orelse
            (unprovisioned =:= NextState andalso 'Rejected' =:= Status)) andalso
        assert_callresult(Data#data.station_cip);
postcondition_booting(From, To, _Data, Call, Result) ->
    ?debugFmt(
        "[postcondition:booting] ~p -> ~p via ~p. Result = ~p",
        [From, To, Call, Result]
    ),
    false.

postcondition_pending(
    From,
    From,
    #data{csms_cip = undefined},
    {call, _, csms_call, [_, MessageID, Message, _]},
    Result
) ->
    ConfigMessages = [
        ~"SetVariablesRequest",
        ~"GetVariablesRequest",
        ~"GetReportRequest",
        ~"GetBaseReportRequest",
        ~"TriggerMessageRequest"
    ],
    case lists:member(ocpp_message:type(Message), ConfigMessages) of
        true ->
            Result =:= ok andalso assert_call(MessageID);
        false ->
            ?debugFmt("illegal csms_call postcondition ~p (~p)", [MessageID, Result]),
            refute_rpcsend() andalso Result =:= {error, badaction}
    end;
postcondition_pending(
    From, From, #data{csms_cip = {RPCCall, _}}, {call, _, csms_call, _}, {error, {call_pending, ID}}
) ->
    refute_rpcsend() andalso ocpp_rpc:id(RPCCall) =:= ID;
postcondition_pending(
    From, From, _Data, {call, _, station_call_triggered, [_, RPCCall]}, ok
) ->
    refute_rpcsend();
postcondition_pending(
    {_, connected, _} = From, From, _Data, {call, _, station_call_security_error, [_, RPCCall]}, ok
) ->
    assert_callerror(RPCCall, 'SecurityError');
postcondition_pending(
    {_, connected, _} = From,
    From,
    Data,
    {call, _, csms_reply, [_, MessageID, Message]},
    ok
) ->
    ocpp_rpc:id(Data#data.station_cip) =:= MessageID andalso
        ocpp_message:action(Message) =:= ocpp_rpc:action(Data#data.station_cip) andalso
        assert_callresult(Data#data.station_cip);
postcondition_pending(
    {_, connected, _} = From,
    From,
    _Data,
    {call, _, csms_reply_badid, [_, MessageID, _]},
    {error, {call_not_pending, MessageID}}
) ->
    refute_rpcsend();
postcondition_pending(
    {_, connected, _} = From,
    From,
    _Data,
    {call, _, csms_reply_badaction, _},
    {error, badaction}
) ->
    refute_rpcsend();
postcondition_pending(_From, _To, _Data, _Call, _Result) ->
    false.

postcondition_accepted(_From, _To, _Data, _Call, _Result) ->
    false.

%%% Helpers and common command generators

%% Returns a unique message ID
messageid() ->
    integer_to_binary(erlang:unique_integer([positive]), 36).

%% Generates an opaque CALL record with the given action as its
%% payload and a unique message ID.
rpccall(Action) when is_binary(Action) ->
    ?LET(
        Payload,
        ocpp_message_gen:message('2.0.1', <<Action/binary, "Request">>),
        ocpp_rpc:call(Payload, messageid())
    );
rpccall(PayloadGen) ->
    ?LET(Payload, PayloadGen, ocpp_rpc:call(Payload, messageid())).

%% Generates an opaque CALLRESULT record with the given action as its
%% payload and a unique message ID.
rpcreply(Action) when is_binary(Action) ->
    ?LET(
        Payload,
        ocpp_message_gen:message('2.0.1', <<Action/binary, "Response">>),
        ocpp_rpc:callresult(Payload, messageid())
    );
rpcreply(PayloadGen) ->
    ?LET(Payload, PayloadGen, ocpp_rpc:callresult(Payload, messageid())).

%% Returns a list of symbolic calls representing both CALL and
%% CALLRESULT send commands (shim functions `csms_call` and
%% ``csms_reply`) from the CSMS to the station that DO NOT result in a
%% sate change and do not conflict with other calls that have
%% "non-normal" behavior in the current state. For example, in the
%% pending state, all calls except configuration calls are forbidden.
%% For that state this function will retun a symbolic call with
%% GetVariables, SetVariables, TriggerMessage, GetReport, and
%% GetBaseReport excluded from the set of possible messages that can
%% be generated.
csms_call_reply(unprovisioned, _ConnectionState, _StationState, Data) ->
    [
        {history,
            {call, station201_shim, csms_call, [
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1'), make_ref()
            ]}},
        {history,
            {call, station201_shim, csms_reply, [
                ?STATIONID, messageid(), ocpp_message_gen:response('2.0.1')
            ]}}
        | csms_call_timeout(Data)
    ];
csms_call_reply(booting, connected, StationState, Data) ->
    [
        {
            {accepted, connected, StationState},
            {call, station201_shim, csms_reply, [
                ?STATIONID,
                ocpp_rpc:id(Data#data.station_cip),
                ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse", [
                    {override, #{status => 'Accepted'}}
                ])
            ]}
        },
        {
            {pending, connected, StationState},
            {call, station201_shim, csms_reply, [
                ?STATIONID,
                ocpp_rpc:id(Data#data.station_cip),
                ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse", [
                    {override, #{status => 'Pending'}}
                ])
            ]}
        },
        {
            {unprovisioned, connected, StationState},
            {call, station201_shim, csms_reply, [
                ?STATIONID,
                ocpp_rpc:id(Data#data.station_cip),
                ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse", [
                    {override, #{status => 'Rejected'}}
                ])
            ]}
        },
        {history,
            {call, station201_shim, csms_reply, [
                ?STATIONID,
                oneof([ocpp_rpc:id(Data#data.station_cip), messageid()]),
                frequency([
                    {1, ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse")},
                    {10,
                        ?SUCHTHAT(
                            Response,
                            ocpp_message_gen:response('2.0.1'),
                            ocpp_message:action(Response) =/= ~"BootNotification"
                        )}
                ])
            ]}},
        {history,
            {call, station201_shim, csms_call, [
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1'), make_ref()
            ]}}
        | csms_call_timeout(Data)
    ];
csms_call_reply(booting, disconnected, _StationState, Data) ->
    [
        %% reply that does not match the ID of the pending CALL
        {history,
            {call, station201_shim, csms_reply, [
                ?STATIONID,
                oneof([messageid(), ocpp_rpc:id(Data#data.station_cip)]),
                frequency([
                    {1, ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse")},
                    {10,
                        ?SUCHTHAT(
                            Response,
                            ocpp_message_gen:response('2.0.1'),
                            ocpp_message:action(Response) =/= ~"BootNotification"
                        )}
                ])
            ]}},
        {history,
            {call, station201_shim, csms_call, [
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1'), make_ref()
            ]}}
        | csms_call_timeout(Data)
    ];
csms_call_reply(pending, connected, _StationState, #data{csms_cip = undefined} = Data) ->
    ConfigMessages = [
        ~"SetVariablesRequest",
        ~"GetVariablesRequest",
        ~"GetReportRequest",
        ~"GetBaseReportRequest",
        ~"TriggerMessageRequest"
    ],
    csms_reply(Data) ++
        [
            {history,
                {call, station201_shim, csms_call, [
                    ?STATIONID,
                    messageid(),
                    frequency([
                        {100, trigger_message_request()},
                        {5,
                            ?LET(
                                Message,
                                oneof([
                                    ~"SetVariablesRequest",
                                    ~"GetVariablesRequest",
                                    ~"GetReportRequest",
                                    ~"GetBaseReportRequest"
                                ]),
                                ocpp_message_gen:message('2.0.1', Message)
                            )},
                        {1,
                            ?SUCHTHAT(
                                Message,
                                ocpp_message_gen:request('2.0.1'),
                                not lists:member(ocpp_message:type(Message), ConfigMessages)
                            )}
                    ]),
                    make_ref()
                ]}}
            | csms_call_timeout(Data)
        ];
csms_call_reply(pending, disconnected, _StationState, #data{csms_cip = undefined} = Data) ->
    [
        {history,
            {call, station201_shim, csms_call, [
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1'), make_ref()
            ]}}
        | csms_call_timeout(Data)
    ];
csms_call_reply(pending, _, _StationState, #data{csms_cip = {RPCCall, Ref}} = Data) ->
    [
        {history,
            {call, station201_shim, csms_call, [
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1'), make_ref()
            ]}},
        {history, {call, station201_shim, csms_rpccall_timeout, [?STATIONID, RPCCall, Ref]}}
        | csms_call_timeout(Data)
    ].

csms_reply(#data{station_cip = undefined} = Data) ->
    csms_badreply(Data);
csms_reply(#data{station_cip = CiP} = Data) ->
    Action = ocpp_rpc:action(CiP),
    [
        {history,
            {call, station201_shim, csms_reply, [
                ?STATIONID,
                ocpp_rpc:id(CiP),
                ocpp_message_gen:message('2.0.1', <<Action/binary, "Response">>)
            ]}}
        | csms_badreply(Data)
    ].

csms_badreply(#data{station_cip = undefined}) ->
    [
        {history,
            {call, station201_shim, csms_reply_badid, [
                ?STATIONID, messageid(), ocpp_message_gen:response('2.0.1')
            ]}}
    ];
csms_badreply(#data{station_cip = CiP}) ->
    [
        {history,
            {call, station201_shim, csms_reply_badid, [
                ?STATIONID, messageid(), ocpp_message_gen:response('2.0.1')
            ]}},
        {history,
            {call, station201_shim, csms_reply_badaction, [
                ?STATIONID,
                ocpp_rpc:id(CiP),
                ?SUCHTHAT(
                    Message,
                    ocpp_message_gen:response('2.0.1'),
                    ocpp_message:action(Message) =/= ocpp_rpc:action(CiP)
                )
            ]}}
    ].

trigger_message_request() ->
    ?LET(
        M,
        oneof([
            'BootNotification',
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
        ]),
        case M of
            'MeterValues' ->
                ocpp_message_gen:message(
                    '2.0.1',
                    ~"TriggerMessageRequest",
                    [
                        {override, #{
                            requestedMessage => M, evse => ?LET(ID, non_neg_integer(), #{id => ID})
                        }}
                    ]
                );
            'StatusNotification' ->
                ocpp_message_gen:message(
                    '2.0.1',
                    ~"TriggerMessageRequest",
                    [
                        {override, #{
                            requestedMessage => M,
                            evse => #{
                                id => non_neg_integer(),
                                connectorId => pos_integer()
                            }
                        }}
                    ]
                );
            _ ->
                ocpp_message_gen:message(
                    '2.0.1',
                    ~"TriggerMessageRequest",
                    [{without, [evse]}, {override, #{requestedMessage => M}}]
                )
        end
    ).

csms_call_timeout(#data{csms_cip = {RPCCall, TRef}, csms_timedout = TimedOutRPC}) ->
    %% TODO include a call with an old ref from an answered call, not just a random one
    [
        {history,
            frequency(
                [
                    {15,
                        {call, station201_shim, csms_rpccall_timeout, [?STATIONID, RPCCall, TRef]}},
                    {1,
                        {call, station201_shim, csms_rpccall_timeout, [
                            ?STATIONID, rpccall(ocpp_message_gen:request('2.0.1')), make_ref()
                        ]}}
                ]
            )}
    ];
csms_call_timeout(_) ->
    %% TODO include a call with an old ref, not just a random one
    [
        {history,
            {call, station201_shim, csms_rpccall_timeout, [
                ?STATIONID, rpccall(ocpp_message_gen:request('2.0.1')), make_ref()
            ]}}
    ].

%% A reply that is not associated with any pending call from the CSMS
-define(ARBITRARY_STATION_REPLY,
    {call, station201_shim, station_reply_badid, [
        ?STATIONID,
        %% NOTE This term is used only for its message ID, the
        %%      action does not need to match the action in the
        %%      response in this case. Filling in a dummy message
        %%      here prevents pollution of the shim's API (it is
        %%      bad enough as it is) and keeps the pre/post
        %%      conditions as simple and concise as possible.
        ?LET(
            Payload,
            ocpp_message_gen:message('2.0.1', ~"TriggerMessageRequest"),
            ocpp_rpc:call(Payload, messageid())
        ),
        ocpp_message_gen:response('2.0.1')
    ]}
).

%% Same as `csms_call_reply/4`, but for calls from the station to the CSMS.
station_call_reply(unprovisioned, connected, {up, _}, _Data) ->
    [
        {
            {booting, connected, {up, clean}},
            {call, station201_shim, station_call, [
                ?STATIONID, rpccall(~"BootNotification")
            ]}
        },
        {history,
            {call, station201_shim, station_call_security_error, [
                ?STATIONID,
                rpccall(
                    ?SUCHTHAT(
                        Message,
                        ocpp_message_gen:request('2.0.1'),
                        ocpp_message:action(Message) =/= ~"BootNotification"
                    )
                )
            ]}},
        {history, ?ARBITRARY_STATION_REPLY}
    ];
station_call_reply(unprovisioned, disconnected, _StationState, _Data) ->
    [
        {history,
            {call, station201_shim, station_call, [
                ?STATIONID, rpccall(ocpp_message_gen:request('2.0.1'))
            ]}},
        {history, ?ARBITRARY_STATION_REPLY}
    ];
station_call_reply(booting, disconnected, _StationState, _Data) ->
    [
        {history,
            {call, station201_shim, station_call, [
                ?STATIONID,
                rpccall(ocpp_message_gen:request('2.0.1'))
            ]}},
        {history, ?ARBITRARY_STATION_REPLY}
    ];
station_call_reply(booting, connected, StationState, _Data) ->
    [
        {
            {booting, connected, {up, clean}},
            {call, station201_shim, station_call, [
                ?STATIONID, rpccall(~"BootNotification")
            ]}
        },
        {
            {unprovisioned, connected, StationState},
            {call, station201_shim, station_call_security_error, [
                ?STATIONID,
                rpccall(
                    ?SUCHTHAT(
                        Message,
                        ocpp_message_gen:request('2.0.1'),
                        ocpp_message:action(Message) =/= ~"BootNotification"
                    )
                )
            ]}
        },
        {history, ?ARBITRARY_STATION_REPLY}
    ];
station_call_reply(pending, disconnected, _StationState, _Data) ->
    [
        {history,
            {call, station201_shim, station_call, [
                ?STATIONID, rpccall(ocpp_message_gen:request('2.0.1'))
            ]}},
        %% TODO send triggered messages and reports
        {history, ?ARBITRARY_STATION_REPLY}
    ];
station_call_reply(
    pending, connected, _StationState, Data = #data{triggers = {Accepted, _Rejected}}
) ->
    TriggeredActions = lists:uniq([
        TriggeredAction
     || {TriggeredAction, _} <- Accepted
    ]),
    RequestedReports = [RequestID || {_, RequestID} <- Data#data.report_requests],
    station_reply(Data) ++
        station_call_triggered(Data) ++
        [
            {
                {booting, connected, {up, clean}},
                {call, station201_shim, station_call, [?STATIONID, rpccall(~"BootNotification")]}
            },
            {history,
                {call, station201_shim, station_call_security_error, [
                    ?STATIONID,
                    rpccall(
                        ?SUCHTHAT(
                            Message,
                            ocpp_message_gen:request('2.0.1'),
                            %% Not a triggered or otherwise allowed message
                            not lists:member(
                                ocpp_message:action(Message),
                                [
                                    ~"BootNotification", ~"NotifiyReport" | TriggeredActions
                                ]
                            ) orelse
                                %% Not a requested report
                                (ocpp_message:action(Message) =:= ~"NotifyReport" andalso
                                    not lists:member(
                                        ocpp_message:get(requestId, Message), RequestedReports
                                    ))
                        )
                    )
                ]}}
        ].

station_call_triggered(#data{triggers = {[], _}}) ->
    %% TODO do a rejected triggered call
    [];
station_call_triggered(#data{triggers = {Accepted, _Rejected}}) ->
    [
        {history,
            {call, station201_shim, station_call_triggered, [
                ?STATIONID,
                ?LET(
                    {Action, EVSE},
                    oneof(Accepted),
                    begin
                        Gen =
                            case Action of
                                ~"MeterValues" ->
                                    ocpp_message_gen:message(
                                        '2.0.1',
                                        ~"MeterValuesRequest",
                                        [{override, convert_evse_id_fields(EVSE)}]
                                    );
                                ~"StatusNotification" ->
                                    ocpp_message_gen:message(
                                        '2.0.1',
                                        ~"StatusNotificationRequest",
                                        [{override, convert_evse_id_fields(EVSE)}]
                                    );
                                ~"TransactionEvent" when EVSE =/= undefined ->
                                    ocpp_message_gen:message(
                                        '2.0.1',
                                        ~"TransactionEventRequest",
                                        [{override, #{evse => EVSE}}]
                                    );
                                ~"TransactionEvent" when EVSE =:= undefined ->
                                    ocpp_message_gen:message(
                                        '2.0.1',
                                        ~"TransactionEventRequest",
                                        [{without, [evse]}]
                                    );
                                ~"SignChargingStationCertificate" ->
                                    ocpp_message_gen:message(
                                        '2.0.1',
                                        ~"SignCertificateRequest",
                                        [
                                            {override, #{
                                                certificateType => 'ChargingStationCertificate'
                                            }}
                                        ]
                                    );
                                ~"SignV2GCertificate" ->
                                    ocpp_message_gen:message(
                                        '2.0.1',
                                        ~"SignCertificateRequest",
                                        [{override, #{certificateType => 'V2GCertificate'}}]
                                    );
                                ~"SignCombinedCertificate" ->
                                    ocpp_message_gen:message(
                                        '2.0.1',
                                        ~"SignCertificateRequest",
                                        [{without, [certificateType]}]
                                    );
                                _ ->
                                    ocpp_message_gen:message('2.0.1', Action)
                            end,
                        rpccall(Gen)
                    end
                )
            ]}}
    ].

convert_evse_id_fields(EVSE) ->
    maps:fold(
        fun
            (id, V, Acc) -> Acc#{evseId => V};
            (K, V, Acc) -> Acc#{K => V}
        end,
        #{},
        EVSE
    ).

station_reply(#data{csms_cip = undefined}) ->
    [{history, ?ARBITRARY_STATION_REPLY}];
station_reply(#data{csms_cip = {RPCCall, _TimerRef}, csms_timedout = TimedOut}) ->
    CallAction = ocpp_rpc:action(RPCCall),
    [
        {history,
            {call, station201_shim, station_reply, [
                ?STATIONID,
                RPCCall,
                response(CallAction)
            ]}},
        {history, ?ARBITRARY_STATION_REPLY},
        {history,
            {call, station201_shim, station_reply_badaction, [
                ?STATIONID,
                RPCCall,
                ?SUCHTHAT(
                    Message,
                    ocpp_message_gen:response('2.0.1'),
                    ocpp_message:action(Message) =/= CallAction andalso
                        %% filter out other message instances that can succesfully decode as a CallAction
                        case
                            ocpp_rpc:decode(
                                '2.0.1',
                                ocpp_rpc:encode(ocpp_rpc:callresult(Message, ocpp_rpc:id(RPCCall))),
                                [{expected, CallAction}]
                            )
                        of
                            {ok, {callresult, _}} -> false;
                            _ -> true
                        end
                )
            ]}}
        | if
            TimedOut =:= undefined ->
                [];
            true ->
                TOAction = ocpp_rpc:action(TimedOut),
                [
                    {history,
                        {call, station201_shim, station_reply_timedout, [
                            ?STATIONID,
                            TimedOut,
                            ocpp_message_gen:message(
                                '2.0.1', <<TOAction/binary, "Response">>
                            )
                        ]}}
                ]
        end
    ].

response(~"TriggerMessage") ->
    ?LET(
        Status,
        frequency([{20, 'Accepted'}, {1, 'Rejected'}, {1, 'NotImplemented'}]),
        ocpp_message_gen:message(
            '2.0.1', ~"TriggerMessageResponse", [{override, #{status => Status}}]
        )
    );
response(CallAction) ->
    ocpp_message_gen:message(
        '2.0.1', <<CallAction/binary, "Response">>
    ).

connect_disconnect_power(AppState, connected, StationState) ->
    [
        {
            {AppState, disconnected, StationState},
            {call, station201_shim, station_disconnect, [?STATIONID]}
        },
        {history, ?CALL_CONNECT_SUPPORTED},
        {history, ?CALL_CONNECT_UNSUPPORTED}
    ];
connect_disconnect_power(AppState, disconnected, {up, _} = StationState) ->
    [
        {history, {call, station201_shim, station_disconnect, [?STATIONID]}},
        {{AppState, connected, StationState}, ?CALL_CONNECT_SUPPORTED},
        {history, ?CALL_CONNECT_UNSUPPORTED},
        {{AppState, connected, {up, dirty}}, {call, station201_shim, station_power_cycle, []}}
    ].

version_list(supported) ->
    ?SUCHTHAT(
        Versions,
        list(oneof(['1.6', '2.0.1', '2.1'])),
        lists:member('2.0.1', Versions)
    );
version_list(unsupported) ->
    list(oneof(['1.6', '2.1'])).

%%% assertions

refute_rpcsend() ->
    receive
        {ocpp, {rpcsend, _}} ->
            false
    after 50 ->
        true
    end.

assert_call(ID) ->
    receive
        {ocpp, {rpcsend, CallBin}} ->
            case ocpp_rpc:decode('2.0.1', CallBin, []) of
                {ok, {call, Call}} ->
                    ocpp_rpc:id(Call) =:= ID;
                _ ->
                    false
            end
    after 100 ->
        false
    end.

assert_callresult(RPCCall) ->
    receive
        {ocpp, {rpcsend, ReplyBin}} ->
            Action = ocpp_rpc:action(RPCCall),
            case ocpp_rpc:decode('2.0.1', ReplyBin, [{expected, Action}]) of
                {ok, {callresult, CallResult}} ->
                    ocpp_rpc:id(CallResult) =:= ocpp_rpc:id(RPCCall);
                _ ->
                    false
            end
    after 100 ->
        false
    end.

assert_callerror(RPCCall, ErrorCode) ->
    receive
        {ocpp, {rpcsend, ReplyBin}} ->
            Action = ocpp_rpc:action(RPCCall),
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
