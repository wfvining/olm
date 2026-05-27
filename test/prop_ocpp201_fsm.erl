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
    station_cip :: ocpp_rpc:call() | undefined
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
    connect_disconnect_power(?FUNCTION_NAME, connected, StationState);
%% csms_call_reply(pending, ConnectionState, StationState, Data) ++
%%     station_call_reply(pending, ConnectionState, StationState, Data) ++

pending(disconnected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, disconnected, StationState).

accepted(connected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, connected, StationState);
%% csms_call_reply(accepted, ConnectionState, StationState, Data) ++
%%     station_call_reply(accepted, ConnectionState, StationState, Data) ++

accepted(disconnected, StationState, Data) ->
    connect_disconnect_power(?FUNCTION_NAME, disconnected, StationState).

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
    NextState =/= booting
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
    {call, _, station_call, [_, RPCCall]}
) ->
    %% any message other than a BootNotification MUST change the state to unprovisioned
    ocpp_rpc:action(RPCCall) =/= ~"BootNotification";
precondition(From, From, _Data, {call, _, station_call, [_, RPCCall]}) ->
    ocpp_rpc:action(RPCCall) =/= ~"BootNotification";
precondition({_, ConnState, _} = From, From, _Data, {call, _, Fun, _}) when
    Fun =:= csms_call;
    Fun =:= csms_reply;
    Fun =:= station_reply;
    Fun =:= connect_unsupported;
    Fun =:= connect_supported, ConnState =:= connected
->
    true;
precondition(_From, _To, _Data, _Call) ->
    false.

weight({unprovisioned, _, _}, {booting, _, _}, _) ->
    3;
weight({_, disconnected, _}, {_, connected, _}, _) ->
    2;
weight(_From, _To, _Call) ->
    1.

next_state_data(
    {_, connected, {up, _}},
    {booting, connected, {up, clean}},
    Data,
    _Result,
    {call, _, station_call, [_, RPCCall]}
) ->
    Data#data{station_cip = RPCCall};
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
next_state_data(_From, _To, Data, _Result, _Call) ->
    Data.

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
    {error, bad_message}
) when
    CiP =/= undefined
->
    MessageID =:= ocpp_rpc:id(CiP) andalso
        ocpp_rpc:action(CiP) =/= ocpp_message:action(Payload) andalso
        refute_rpcsend();
postcondition(
    {_, disconnected, _} = From, From, _Data, {call, _, Fun, _}, {error, not_connected}
) when
    Fun =:= station_call; Fun =:= station_reply
->
    true;
postcondition({_, connected, _} = From, From, _Data, {call, _, station_reply, _}, ok) ->
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
    {_, connected, _} = From, From, _Data, {call, _, station_call, [_, RPCCall]}, ok
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
    {call, _, station_call, [_, RPCCall]},
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
csms_call_reply(unprovisioned, _ConnectionState, _StationState, _Data) ->
    [
        {history,
            {call, station201_shim, csms_call, [
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1')
            ]}},
        {history,
            {call, station201_shim, csms_reply, [
                ?STATIONID, messageid(), ocpp_message_gen:response('2.0.1')
            ]}}
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
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1')
            ]}}
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
                ?STATIONID, messageid(), ocpp_message_gen:request('2.0.1')
            ]}}
        %% ;
    ].
%% csms_call_reply(pending, connected, _StationState, Data) ->
%%     %% TODO
%%     [{history, {call, station201_shim, csms_call, [?STATIONID, messageid(), ocpp_message_gen:request('2.0.1')]}},
%%      {history, {call, station201_shim, csms_reply, [?STATIONID, messageid(), ocpp_message_gen:response('2.0.1')]}}].

%% A reply that is not associated with any pending call from the CSMS
-define(ARBITRARY_STATION_REPLY,
    {call, station201_shim, station_reply, [
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
            {call, station201_shim, station_call, [
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
            {call, station201_shim, station_call, [
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
    ].


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
