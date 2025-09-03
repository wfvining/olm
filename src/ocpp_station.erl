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
-export([connect/2, rpc/2, reply/3, call/3]).
-export([init/1, callback_mode/0]).
%% State callbacks
-export([
    unprovisioned/3,
    connected/3,
    provisioning/3,
    boot_pending/3,
    accepted/3,
    offline/3,
    reconnecting/3
]).

-ifdef(TEST).
-export([trace/1]).
-endif.

-define(registry(StationID), {via, gproc, ?name(StationID)}).
-define(name(Name), {n, l, {ocpp_station, Name, station}}).

-record(state, {
    stationid :: binary(),
    connection ::
        {pending, gen_statem:from(), pid()} | {ocpp:version(), pid(), reference()} | undefined,
    pending_call :: {binary(), binary(), ocpp_message:message()} | undefined,
    rpc_call :: undefined | ocpp_rpc:call(),
    triggered = #{} :: #{binary() => [ocpp_message:message()]}
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
-spec rpc(binary(), binary()) -> ok | {error, Reason :: any()}.
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
    gen_statem:call(?registry(StationID), {send, {callresult, MessageID, Payload}}).

-doc """
Send an RPC CALL message to the station.
""".
-spec call(StationID :: binary(), MessageID :: binary(), Payload :: ocpp_message:message()) ->
    ok | {error, disconnected | {badstate, State :: atom()}}.
call(StationID, MessageID, Payload) ->
    gen_statem:call(?registry(StationID), {send, {call, MessageID, Payload}}).

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
connected({call, From}, {rpc, Pid1, _}, #state{connection = {_, Pid2, _}}) when Pid1 =/= Pid2 ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
connected(
    {call, From},
    {rpc, Pid, RPCBinary},
    State
) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, RPCCall}} ->
            Payload = ocpp_rpc:payload(RPCCall),
            case ocpp_message:type(Payload) of
                'BootNotificationRequest' ->
                    ocpp_handler:rpc_call(
                        State#state.stationid,
                        ocpp_message:type(Payload),
                        ocpp_rpc:id(RPCCall),
                        Payload
                    ),
                    {next_state, provisioning, State#state{rpc_call = RPCCall}, [
                        {reply, From, ok}
                    ]};
                MessageType ->
                    logger:warning(
                        "~p got unexpected ~p message before provisioning (MessageID = ~p)",
                        [State#state.stationid, MessageType, ocpp_rpc:id(RPCCall)]
                    ),
                    {keep_state_and_data, [
                        {reply, From, {error, not_provisioned}}
                    ]}
            end;
        {ok, Message} ->
            logger:warning(
                "~p got unexpected RPC message before provisioning~nMessage = ~p",
                [State#state.stationid, Message]
            ),
            {keep_state_and_data, [{reply, From, {error, not_provisioned}}]};
        {error, {_, Reason}} ->
            logger:warning(
                "~p got unexpected invalid RPC message before provisioning.~n"
                "Message = ~p~nReason = ~p",
                [State#state.stationid, RPCBinary, Reason]
            ),
            {keep_state_and_data, [{reply, From, {error, Reason}}]}
    end;
connected({call, From}, {send, _}, _) ->
    {keep_state_and_data, [{reply, From, {error, not_provisioned}}]}.

provisioning({call, From}, {rpc, Pid, RPCBinary}, State = #state{rpc_call = PendingCall}) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, RPCCall}} ->
            Payload = ocpp_rpc:payload(RPCCall),
            MessageID = ocpp_rpc:id(RPCCall),
            PendingID = ocpp_rpc:id(PendingCall),
            case ocpp_message:type(Payload) of
                'BootNotificationRequest' when MessageID =/= PendingID ->
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
                        State#state.stationid,
                        ocpp_rpc:id(RPCCall),
                        ocpp_message:type(Payload),
                        Payload
                    ),
                    {next_state, provisioning, State#state{rpc_call = RPCCall}, [
                        {reply, From, ok}
                    ]};
                'BootNotificationRequest' when MessageID =:= PendingID ->
                    logger:warning(
                        "Received invalid BootNotificationRequest with duplicate message ID: ~p",
                        [MessageID]
                    ),
                    {keep_state_and_data, [
                        {reply, From, {error, duplicate_message}}
                    ]};
                MessageType ->
                    logger:warning(
                        "~p got unexpected ~p message before provisioning (MessageID = ~p)",
                        [State#state.stationid, MessageType, ocpp_rpc:id(RPCCall)]
                    ),
                    {keep_state_and_data, [
                        {reply, From, {error, not_provisioned}}
                    ]}
            end;
        {error, {_, Reason}} ->
            logger:warning(
                "~p got unexpected invalid RPC message before provisioning.~n"
                "Message = ~p~nReason = ~p",
                [State#state.stationid, RPCBinary, Reason]
            ),
            {keep_state_and_data, [{reply, From, {error, Reason}}]}
    end;
provisioning(
    {call, From}, {send, {callresult, MessageID, Message}}, #state{rpc_call = PendingCall} = State
) ->
    case ocpp_rpc:id(PendingCall) of
        PendingID when MessageID =:= PendingID ->
            %% the pending call must be a boot notification here.
            RPC = ocpp_rpc:callresult(Message, MessageID),
            rpcsend(State#state.connection, RPC),
            try ocpp_message:get(status, Message) of
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
                        [MessageID, Message]
                    ),
                    {keep_state_and_data, [{reply, From, {error, bad_message}}]}
            end;
        PendingID when MessageID =/= PendingID ->
            logger:warning(
                "Got response for message ~p, but ~p is pending~nresponse dropped.",
                [MessageID, PendingID]
            ),
            {keep_state_and_data, [{reply, From, {error, call_dropped}}]}
    end;
provisioning(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Connection process ~p down before BootNotificationResponse sent~n~p", [Pid, Reason]),
    {next_state, unprovisioned, State#state{connection = undefined, rpc_call = undefined}}.

boot_pending(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Connection process ~p down while boot Pending~n~p", [Pid, Reason]),
    {next_state, unprovisioned, State#state{connection = undefined, rpc_call = undefined}};
boot_pending({call, From}, {rpc, Pid, RPCBinary}, State) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, RPC}} ->
            handle_call_pending(RPC, From, State);
        {ok, {callresult, RPC}} ->
            handle_callresult(RPC, From, State)
    end;
boot_pending({call, From}, {send, {call, MessageID, Message}}, State) ->
    %% TODO prevent illegal messages from being sent.
    RPC = ocpp_rpc:call(Message, MessageID),
    case do_rpc_call(RPC, State) of
        {ok, {Reply, NewState}} ->
            {keep_state, NewState, [{reply, From, Reply}]};
        {error, Reason} ->
            {keep_state, State, [{reply, From, Reason}]}
    end;
boot_pending(
    {call, From}, {send, {callresult, MessageID, Message}}, #state{rpc_call = PendingCall} = State
) ->
    case ocpp_rpc:id(PendingCall) of
        MessageID ->
            RPC = ocpp_rpc:callresult(Message, MessageID),
            rpcsend(State#state.connection, RPC),
            {keep_state, State#state{rpc_call = undefined}, [{reply, From, ok}]};
        PendingID ->
            logger:info(
                "Request to send CALLRESULT for message ID = ~p, but message ID ~p is pending.~n"
                "Station may have timed out while awaiting response to message ~p.~n"
                "CALLRESULT dropped.",
                [MessageID, PendingID, MessageID]
            ),
            {keep_state_and_data, [{reply, From, {error, call_dropped}}]}
    end.

accepted(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Connection process ~p down~n~p", [Pid, Reason]),
    {next_state, offline, State#state{connection = undefined, rpc_call = undefined}};
accepted({call, From}, {rpc, Pid, RPCBinary}, State) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, RPC}} ->
            handle_call(RPC, From, State);
        {ok, {callresult, RPC}} ->
            handle_callresult(RPC, From, State);
        {error, Reason} ->
            {keep_state_and_data, [{reply, From, Reason}]}
    end;
accepted(
    {call, From}, {send, {callresult, MessageID, Message}}, #state{rpc_call = PendingCall} = State
) ->
    PendingAction = ocpp_message:action(ocpp_rpc:payload(PendingCall)),
    ResultAction = ocpp_message:action(Message),
    case ocpp_rpc:id(PendingCall) of
        MessageID ->
            CallResult = ocpp_rpc:callresult(Message, MessageID),
            rpcsend(State#state.connection, CallResult),
            {keep_state, State#state{rpc_call = undefined}, [{reply, From, ok}]};
        PendingID ->
            logger:warning(
                "Got ~sResponse with ID = ~p, but waiting for ~sResponse with ID ~p~nMessage dropped.",
                [ResultAction, MessageID, PendingAction, PendingID]
            ),
            {keep_state_and_data, [
                {reply, From, {error, message_dropped}}
            ]}
    end.

offline(_, _, _) ->
    todo.

reconnecting(_, _, _) ->
    todo.

do_rpc_call(RPC, #state{pending_call = undefined} = State) ->
    rpcsend(State#state.connection, RPC),
    Message = ocpp_rpc:payload(RPC),
    %% TODO start a timer to time-out the pending call
    {ok,
        {ok, State#state{pending_call = {ocpp_rpc:id(RPC), ocpp_message:action(Message), Message}}}};
do_rpc_call(_RPC, State) ->
    {error, {error, {call_pending, State#state.pending_call}}}.

decode_rpc(RPCBinary, Pid, #state{connection = {Version, Pid, _}} = State) ->
    Opts =
        case State#state.pending_call of
            undefined ->
                [];
            {_MessageID, Action, _} ->
                [{expected, Action}]
        end,
    ocpp_rpc:decode(Version, RPCBinary, Opts).

handle_callresult(RPC, From, #state{pending_call = undefined}) ->
    logger:info("got CALLRESULT (ID = ~p) but there is no pending call~n~p", [
        ocpp_rpc:id(RPC), ocpp_rpc:payload(RPC)
    ]),
    {keep_state_and_data, [
        {reply, From, {error, call_dropped}}
    ]};
handle_callresult(RPC, From, #state{pending_call = {PendingID, _, _PendingCall}} = State) ->
    Message = ocpp_rpc:payload(RPC),
    case ocpp_rpc:id(RPC) of
        ID when ID =:= PendingID ->
            process_callresult(Message, From, State);
        ID when ID =/= PendingID ->
            logger:warning(
                "got CALLRESULT for message ID ~p, but ~p is pending~nmessage dropped",
                [ID, PendingID]
            ),
            {keep_state_and_data, [
                {reply, From, {error, call_dropped}}
            ]}
    end.

process_callresult(Message, From, #state{pending_call = {_, _, PendingCall}} = State) ->
    case ocpp_message:type(Message) of
        'TriggerMessageResponse' ->
            {keep_state,
                State#state{
                    triggered = maybe_add_triggered(Message, PendingCall, State#state.triggered)
                },
                [{reply, From, ok}]};
        _ ->
            todo
    end.

%% Handle CALL messages that arrive while in pending state. This is basically just a
%% pre-filter before passing legal messages to handle_call/2
handle_call_pending(RPC, From, State) ->
    Message = ocpp_rpc:payload(RPC),
    case ocpp_message:action(Message) of
        <<"BootNotification">> ->
            %% new BootNotificaionRequest means we can reset to the start of provisioning.
            {next_state, connected, clear_trigger(<<"BootNotification">>, Message, State), [
                postpone
            ]};
        Action when is_map_key(Action, State#state.triggered) ->
            handle_call(RPC, From, State);
        _ ->
            logger:error(
                "OCPP SecurityError~n"
                "Received disallwed/unsolicited CALL from station while boot pending~n"
                "~p",
                [RPC]
            ),
            rpcsend(
                State#state.connection,
                ocpp_rpc:callerror(
                    'SecurityError',
                    ocpp_rpc:id(RPC),
                    [
                        {description, <<"Disallowed or unsolicited CALL while boot pending">>},
                        {details, #{<<"action">> => ocpp_message:action(ocpp_rpc:payload(RPC))}}
                    ]
                )
            ),
            {keep_state_and_data, [{reply, From, ok}]}
    end.

handle_call(RPC, From, State) ->
    Message = ocpp_rpc:payload(RPC),
    ocpp_handler:rpc_call(
        State#state.stationid, ocpp_message:type(Message), ocpp_rpc:id(RPC), Message
    ),
    {keep_state,
        clear_trigger(ocpp_message:action(Message), Message, State#state{
            rpc_call = RPC
        }), [
            {reply, From, ok}
        ]}.

maybe_add_triggered(TriggerMessageResponse, Request, Triggered) ->
    case ocpp_message:get(status, TriggerMessageResponse) of
        'Accepted' ->
            add_triggered(Request, Triggered);
        'Rejected' ->
            Triggered
    end.

add_triggered(Request, Triggered) ->
    ExpectedAction = atom_to_binary(ocpp_message:get(requestedMessage, Request)),
    maps:update_with(
        ExpectedAction, fun(Xs) -> [Request | Xs] end, [Request], Triggered
    ).

clear_trigger(Action, _Message, #state{triggered = Triggered} = State) when
    is_map_key(Action, Triggered)
->
    %% TODO match the message to the trigger instance.
    NewTriggered =
        case Triggered of
            #{Action := [_]} ->
                maps:without([Action], Triggered);
            #{Action := Instances} ->
                Triggered#{Action => tl(Instances)};
            _ ->
                Triggered
        end,
    State#state{triggered = NewTriggered};
clear_trigger(_, _, State) ->
    State.

rpcsend({_, Conn, _}, RPC) ->
    Conn ! {ocpp, {rpcsend, ocpp_rpc:encode(RPC)}}.

-ifdef(TEST).
%% Set a trace on the station using `m:dbg`.
trace(StationID) ->
    Pid = gproc:whereis_name(?name(StationID)),
    dbg:p(Pid, [m, c]),
    dbg:tp(ocpp_station, x).
-endif.
