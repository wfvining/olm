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

-export([start_link/1, start_link/2, stop/1, whereis/1]).
-export([connect/2, disconnect/1, rpc/2, reply/3, call/3, callresulterror/4]).
-export([init/1, callback_mode/0, terminate/3]).
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
    %% flag indicating the station has been accepted
    status = unprovisioned :: accepted | pending | unprovisioned,
    connection ::
        {pending, gen_statem:from(), pid()} | {ocpp:version(), pid(), reference()} | undefined,
    %% pending call from CSMS to station - awaiting response from station
    pending_call :: {binary(), binary(), ocpp_message:message(), ocpp_timer:timer()} | undefined,
    %% pending call from station to CSMS - awaiting response from handler
    rpc_call :: undefined | ocpp_rpc:call(),
    triggered = #{} :: #{binary() => [ocpp_message:message()]},
    %% Timeout for awaiting a response from the station [ms]
    rpccall_timeout :: pos_integer()
}).

start_link(StationID, Options) ->
    gen_statem:start_link(?registry(StationID), ?MODULE, {StationID, Options}, []).

start_link(StationID) ->
    start_link(StationID, []).

-doc """
Return the Pid of the station named `StationID`. If the station does
not exist raises a `badarg` error.
""".
-spec whereis(StationID :: binary()) -> pid() | noproc.
whereis(StationID) ->
    gproc:where(?name(StationID)).

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
Disconnect the client from the staiton. The calling process *must* be
the currently connected process.
""".
-spec disconnect(binary()) -> ok | {error, not_connected}.
disconnect(StationID) ->
    gen_statem:call(?registry(StationID), {disconnect, self()}).

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

callresulterror(StationID, MessageID, ErrorCode, Options) ->
    todo.

callback_mode() ->
    state_functions.

terminate(_Reason, _StateName, #state{pending_call = {_, _, _, TRef}}) ->
    ocpp_timer:cancel(TRef);
terminate(_Reason, _StateName, _State) ->
    ok.

init({StationID, Options}) ->
    DefaultTimeout = application:get_env(ocpp, rpccall_timeout, 30_000),
    CallTimeout = proplists:get_value(
        rpccall_timeout, Options, DefaultTimeout
    ),
    process_flag(trap_exit, true),
    {ok, unprovisioned, #state{stationid = StationID, rpccall_timeout = CallTimeout}}.

unprovisioned(info, {timeout, _, {rpccall, _}}, _State) ->
    keep_state_and_data;
unprovisioned({call, From}, {connect, Pid, Versions}, State) ->
    do_connect(Pid, From, Versions, State, connected);
unprovisioned({call, From}, {rpc, _, _}, _State) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
unprovisioned({call, From}, _, _State) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]}.

connected(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info(
        "Connection down before provisioning. ConnectionPid=~p, Reason=~p",
        [Pid, Reason]
    ),
    {next_state, unprovisioned, State#state{connection = undefined}};
connected(info, {timeout, _, {rpccall, _}}, _State) ->
    keep_state_and_data;
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
                ~"BootNotificationRequest" ->
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
                        "~p got unexpected ~p message before station accepted (MessageID = ~p)",
                        [State#state.stationid, MessageType, ocpp_rpc:id(RPCCall)]
                    ),
                    rpcsend(
                        State#state.connection,
                        ocpp_rpc:callerror(
                            'SecurityError',
                            ocpp_rpc:id(RPCCall),
                            [
                                {description, ~B"Unexpected CALL before station was Accepted"},
                                {details, #{~B"action" => ocpp_message:action(Payload)}}
                            ]
                        )
                    ),
                    {keep_state_and_data, [{reply, From, ok}]}
            end;
        {ok, Message} ->
            logger:warning(
                "~p got unexpected RPC message before provisioning~nMessage = ~p",
                [State#state.stationid, Message]
            ),
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {_, Reason}} ->
            logger:warning(
                "~p got unexpected/invalid RPC message before provisioning.~n"
                "Message = ~p~nReason = ~p",
                [State#state.stationid, RPCBinary, Reason]
            ),
            {keep_state_and_data, [{reply, From, ok}]}
    end;
connected({call, From}, {send, {call, _, _}}, #state{pending_call = {ID, _, _, _}}) ->
    {keep_state_and_data, [{reply, From, {error, {call_pending, ID}}}]};
connected({call, From}, {send, _}, _) ->
    {keep_state_and_data, [{reply, From, {error, not_provisioned}}]};
connected({call, From}, {disconnect, Pid}, #state{connection = {_, Pid, Ref}} = State) ->
    true = demonitor(Ref),
    {next_state, unprovisioned, State#state{connection = undefined}, [{reply, From, ok}]}.

provisioning({call, From}, {rpc, Pid, RPCBinary}, State = #state{rpc_call = PendingCall}) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, RPCCall}} ->
            Payload = ocpp_rpc:payload(RPCCall),
            MessageID = ocpp_rpc:id(RPCCall),
            PendingID = ocpp_rpc:id(PendingCall),
            case ocpp_message:type(Payload) of
                ~"BootNotificationRequest" when MessageID =/= PendingID ->
                    logger:info(
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
                MessageType ->
                    logger:warning(
                        "~p got unexpected ~p message before station accepted (MessageID = ~p)",
                        [State#state.stationid, MessageType, ocpp_rpc:id(RPCCall)]
                    ),
                    rpcsend(
                        State#state.connection,
                        ocpp_rpc:callerror(
                            'SecurityError',
                            ocpp_rpc:id(RPCCall),
                            [
                                {description, ~"Unexpected CALL before station was Accepted"},
                                {details, #{~"action" => ocpp_message:action(Payload)}}
                            ]
                        )
                    ),
                    {next_state, connected, State#state{rpc_call = undefined}, [{reply, From, ok}]}
            end;
        {ok, {callresult, CallResult}} ->
            logger:warning(
                "got illegal CALLRESULT before BootNotificationResponse has been sent~n"
                "ID: ~s~n"
                "Action: ~s~n"
                "Payload:~n~p",
                [
                    ocpp_rpc:id(CallResult),
                    ocpp_message:action(ocpp_rpc:payload(CallResult)),
                    ocpp_rpc:payload(CallResult)
                ]
            ),
            {keep_state_and_data, [{reply, From, ok}]};
        {error, not_connected} ->
            logger:warning(
                "got RPC from process that is not connected to station ~p~nRPC: ~s",
                [State#state.stationid, RPCBinary]
            ),
            {keep_state_and_data, [{reply, From, {error, not_connected}}]};
        {error, {_, Reason}} ->
            logger:warning(
                "~p got unexpected invalid RPC message before provisioning.~n"
                "Message = ~p~nReason = ~p",
                [State#state.stationid, RPCBinary, Reason]
            ),
            {keep_state_and_data, [{reply, From, ok}]}
    end;
provisioning({call, From}, {send, {call, _, _}}, #state{pending_call = {ID, _, _, _}}) ->
    {keep_state_and_data, [{reply, From, {error, {call_pending, ID}}}]};
provisioning({call, From}, {send, {call, _, _}}, _State) ->
    {keep_state_and_data, [{reply, From, {error, not_provisioned}}]};
provisioning(
    {call, From}, {send, {callresult, MessageID, Message}}, #state{rpc_call = PendingCall} = State
) ->
    ResultAction = ocpp_message:action(Message),
    case ocpp_rpc:id(PendingCall) of
        PendingID when MessageID =:= PendingID, ResultAction =:= ~"BootNotification" ->
            RPC = ocpp_rpc:callresult(Message, MessageID),
            rpcsend(State#state.connection, RPC),
            case ocpp_message:get(status, Message) of
                'Accepted' ->
                    {next_state, accepted, State#state{rpc_call = undefined, status = accepted}, [
                        {reply, From, ok}
                    ]};
                'Rejected' ->
                    {next_state, connected,
                        State#state{
                            rpc_call = undefined, pending_call = undefined, status = unprovisioned
                        },
                        [
                            {reply, From, ok}
                        ]};
                'Pending' ->
                    {next_state, boot_pending, State#state{rpc_call = undefined, status = pending},
                        [
                            {reply, From, ok}
                        ]}
            end;
        PendingID when MessageID =/= PendingID ->
            logger:warning(
                "Got response for message ~p, but ~p is pending~nresponse dropped.",
                [MessageID, PendingID]
            ),
            {keep_state_and_data, [{reply, From, {error, {call_not_pending, MessageID}}}]};
        _ when ResultAction =/= ~"BootNotification" ->
            logger:warning(
                "CALLRESULT(ID=~p) response action (~p) does not match pending call (~p)~n~p",
                [MessageID, ResultAction, ocpp_rpc:action(PendingCall), Message]
            ),
            {keep_state_and_data, [{reply, From, {error, badaction}}]}
    end;
provisioning({call, From}, {connect, _, _}, _) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]};
provisioning(
    info,
    {timeout, Ref, {rpccall, MessageID}},
    #state{pending_call = {MessageID, _, _, Ref}} = State
) ->
    logger:info("RPCCALL to station timed out (MessageID = ~p)", [MessageID]),
    {keep_state, State#state{pending_call = undefined}};
provisioning(info, {timeout, _, {rpccall, _}}, _State) ->
    keep_state_and_data;
provisioning(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Connection process ~p down before BootNotificationResponse sent~n~p", [Pid, Reason]),
    {next_state, offline, State#state{connection = undefined}};
provisioning({call, From}, {disconnect, Pid}, #state{connection = {_, Pid, Ref}} = State) ->
    true = demonitor(Ref),
    {next_state, offline, State#state{connection = undefined}, [
        {reply, From, ok}
    ]}.

boot_pending(
    info,
    {'DOWN', Ref, process, Pid, Reason},
    #state{connection = {_, Pid, Ref}} = State
) ->
    logger:info("Connection process ~p down while boot Pending~n~p", [Pid, Reason]),
    {next_state, offline, State#state{connection = undefined}};
boot_pending({call, From}, {disconnect, Pid}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Station ~p disconnected while boot Pending", [Pid]),
    true = demonitor(Ref),
    {next_state, offline, State#state{connection = undefined}, [
        {reply, From, ok}
    ]};
boot_pending({call, From}, {rpc, Pid, RPCBinary}, State) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, RPC}} ->
            handle_call_pending(RPC, From, State);
        {ok, {callresult, RPC}} ->
            handle_callresult(RPC, From, State);
        {error, {unknown_action, ID}} ->
            logger:info("RPCREPLY to ~p dropped because no CALL is pending", [ID]),
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {badid, ID}} ->
            logger:info(
                "RPCREPLY with ID ~p dropped because CALL ~p is pending",
                [ID, pending_call_id(State)]
            ),
            {keep_state_and_data, [{reply, From, ok}]};
        {error, not_connected} ->
            logger:warning(
                "got RPC from process that is not connected to station ~p~nRPC: ~s",
                [State#state.stationid, RPCBinary]
            ),
            {keep_state_and_data, [{reply, From, {error, not_connected}}]};
        {ok, {callerror, _}} ->
            %% TODO handle callerror response - log, clear pending call (if ID matches),
            %%      eventually need a handler mechanism to report this to the CSMS
            %%      implementation.
            {keep_state_and_data, [{reply, From, ok}]};
        {ok, {callresulterror, _}} ->
            %% TODO handle callresult error - log & drop - may need handler eventually
            {keep_state_and_data, [{reply, From, ok}]};
        {ok, {send, _}} ->
            %% TODO handle send - should be an error in this state (SecurityError?)
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {callerror, _}} ->
            %% TODO this comes from decoding an invalid request - drop, log, & reply
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {callresulterror, _}} ->
            %% TODO this comes from decoding an invalid response - log, send error
            %%      reply, clear call in progress
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {error, Error}} ->
            maybe
                {ID, _, PendingCall, TRef} ?= State#state.pending_call,
                callresulterror ?= ocpp_rpc:error_type(Error),
                ID ?= ocpp_rpc:id(Error),
                logger:warning(
                    "received invalid response for CALL ~p\nPending = ~p\nError = ~p",
                    [ID, PendingCall, Error]
                ),
                ocpp_timer:cancel(TRef),
                {keep_state, State#state{pending_call = undefined}, [{reply, From, ok}]}
            else
                _ ->
                    {keep_state_and_data, [{reply, From, ok}]}
            end
    end;
boot_pending({call, From}, {send, {call, MessageID, Message}}, State) ->
    RPC = ocpp_rpc:call(Message, MessageID),
    case ocpp_message:action(Message) of
        Action when
            Action =:= ~"SetVariables";
            Action =:= ~"GetVariables";
            Action =:= ~"GetReport";
            Action =:= ~"GetBaseReport";
            Action =:= ~"TriggerMessage"
        ->
            case do_rpc_call(RPC, State) of
                {ok, NewState} ->
                    {keep_state, NewState, [{reply, From, ok}]};
                {error, Reason} ->
                    {keep_state, State, [{reply, From, {error, Reason}}]}
            end;
        %% XXX this is a bit ugly - should probably refacor soon. The
        %% concept here is that ocpp_statio:call errors are returned
        %% predictably with a consistent priority:
        %% 1. {error, not_connected}
        %% 2. {error, {call_pending, _}} (if not 1)
        %% 3. {error, badaction} (if not 1 and not 2)
        _ when State#state.pending_call =:= undefined ->
            {keep_state_and_data, [{reply, From, {error, badaction}}]};
        _ when State#state.pending_call =/= undefined ->
            {keep_state_and_data, [
                {reply, From, {error, {call_pending, element(1, State#state.pending_call)}}}
            ]}
    end;
boot_pending({call, From}, {send, {callresult, MessageID, _}}, #state{rpc_call = undefined}) ->
    {keep_state_and_data, [{reply, From, {error, {call_not_pending, MessageID}}}]};
boot_pending(
    {call, From}, {send, {callresult, MessageID, Message}}, #state{rpc_call = PendingCall} = State
) ->
    PendingAction = ocpp_rpc:action(PendingCall),
    ReplyAction = ocpp_message:action(Message),
    case ocpp_rpc:id(PendingCall) of
        MessageID when PendingAction =:= ReplyAction ->
            RPC = ocpp_rpc:callresult(Message, MessageID),
            rpcsend(State#state.connection, RPC),
            {keep_state, State#state{rpc_call = undefined}, [{reply, From, ok}]};
        MessageID ->
            logger:error(
                "Request to send CALLRESULT for message ~p with action ~p, "
                "but CALLRESULT action is ~p",
                [MessageID, PendingAction, ReplyAction]
            ),
            {keep_state_and_data, [{reply, From, {error, badaction}}]};
        PendingID ->
            logger:info(
                "Request to send CALLRESULT for message ID = ~p, but message ID ~p is pending.~n"
                "Station may have timed out while awaiting response to message ~p.~n"
                "CALLRESULT dropped.",
                [MessageID, PendingID, MessageID]
            ),
            {keep_state_and_data, [{reply, From, {error, {call_not_pending, MessageID}}}]}
    end;
boot_pending(
    info,
    {timeout, TRef, {rpccall, MessageID}},
    #state{pending_call = {MessageID, _, _, TRef}} = State
) ->
    logger:info("RPCCALL to station timed out (MessageID = ~p)", [MessageID]),
    {keep_state, State#state{pending_call = undefined}};
boot_pending(info, {timeout, _, {rpccall, _}}, _State) ->
    keep_state_and_data;
boot_pending({call, From}, {connect, _, _}, _State) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]}.

accepted(
    info,
    {timeout, Ref, {rpccall, MessageID}},
    #state{pending_call = {MessageID, _, _, Ref}} = State
) ->
    logger:info("RPCCALL to station timed out (MessageID = ~p)", [MessageID]),
    {keep_state, State#state{pending_call = undefined}};
accepted(info, {timeout, _, {rpccall, _}}, _State) ->
    keep_state_and_data;
accepted(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Connection process ~p down~n~p", [Pid, Reason]),
    {next_state, offline, State#state{connection = undefined}};
accepted({call, From}, {disconnect, Pid}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Station ~p disconnected", [State#state.stationid]),
    demonitor(Ref),
    {next_state, offline, State#state{connection = undefined}, [{reply, From, ok}]};
accepted({call, From}, {connect, _, _}, _State) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]};
accepted({call, From}, {rpc, Pid, RPCBinary}, State) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, RPC}} ->
            handle_call(RPC, From, State);
        {ok, {callresult, RPC}} ->
            handle_callresult(RPC, From, State);
        {error, not_connected} ->
            logger:warning(
                "got RPC from process that is not connected to station ~p~nRPC: ~p",
                [State#state.stationid, RPCBinary]
            ),
            {keep_state_and_data, [{reply, From, {error, not_connected}}]};
        {ok, {callerror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {ok, {callresulterror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {ok, {send, Send}} ->
            handle_call(Send, From, State);
        {error, {unknown_action, ID}} ->
            logger:info("CALLRESULT for unknown action ~p dropped", [ID]),
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {badid, ID}} ->
            logger:info("CALLRESULT for ~p dropped because ~p is pending", [
                ID, pending_call_id(State)
            ]),
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {callerror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {callresulterror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {error, _}} ->
            {keep_state_and_data, [{reply, From, ok}]}
    end;
accepted({call, From}, {send, {callresult, MessageID, _}}, #state{rpc_call = undefined}) ->
    logger:warning("cannot send CALLRESULT for ~p. No CALL is pending.", [MessageID]),
    {keep_state_and_data, [{reply, From, {error, {call_not_pending, MessageID}}}]};
accepted(
    {call, From}, {send, {callresult, MessageID, Message}}, #state{rpc_call = PendingCall} = State
) ->
    PendingAction = ocpp_message:action(ocpp_rpc:payload(PendingCall)),
    ResultAction = ocpp_message:action(Message),
    case ocpp_rpc:id(PendingCall) of
        MessageID when PendingAction =:= ResultAction ->
            CallResult = ocpp_rpc:callresult(Message, MessageID),
            rpcsend(State#state.connection, CallResult),
            {keep_state, State#state{rpc_call = undefined}, [{reply, From, ok}]};
        MessageID ->
            logger:error(
                "Request to send CALLRESULT for message ~p with action ~p, "
                "but CALLRESULT action is ~p",
                [MessageID, PendingAction, ResultAction]
            ),
            {keep_state_and_data, [{reply, From, {error, badaction}}]};
        PendingID ->
            logger:warning(
                "Got ~sResponse with ID = ~p, but waiting for ~sResponse with ID ~p~nMessage dropped.",
                [ResultAction, MessageID, PendingAction, PendingID]
            ),
            {keep_state_and_data, [
                {reply, From, {error, {call_not_pending, MessageID}}}
            ]}
    end;
accepted({call, From}, {send, {call, MessageID, Message}}, State) ->
    RPC = ocpp_rpc:call(Message, MessageID),
    case do_rpc_call(RPC, State) of
        {ok, NewState} ->
            {keep_state, NewState, [{reply, From, ok}]};
        {error, Reason} ->
            {keep_state, State, [{reply, From, {error, Reason}}]}
    end.

offline(
    info,
    {timeout, TRef, {rpccall, MessageID}},
    #state{pending_call = {MessageID, _, _, TRef}} = State
) ->
    logger:info("RPCCALL to station timed out (MessageID = ~p)", [MessageID]),
    {keep_state, State#state{pending_call = undefined}};
offline(info, {timeout, _, {rpccall, _}}, _State) ->
    keep_state_and_data;
offline({call, From}, {rpc, Pid, RPCBinary}, State) ->
    logger:warning(
        "station ~p got RPC from ~p while in 'offline' state~nRPC: ~p",
        [State#state.stationid, Pid, RPCBinary]
    ),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
offline({call, From}, {send, {call, ID, Message}}, State) ->
    RPC = ocpp_rpc:call(Message, ID),
    {error, Reason} = do_rpc_call(RPC, State),
    {keep_state_and_data, [{reply, From, {error, Reason}}]};
offline({call, From}, {send, {callresult, _RPCCall, _Response}}, _State) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
offline({call, From}, {connect, Pid, Versions}, #state{status = accepted} = State) ->
    do_connect(Pid, From, Versions, State, reconnecting);
offline(
    {call, From},
    {connect, Pid, Versions},
    State = #state{rpc_call = undefined, status = unprovisioned}
) ->
    do_connect(Pid, From, Versions, State, connected);
offline(
    {call, From}, {connect, Pid, Versions}, State = #state{rpc_call = undefined, status = pending}
) ->
    do_connect(Pid, From, Versions, State, boot_pending);
offline({call, From}, {connect, Pid, Versions}, State = #state{rpc_call = RPCCall}) ->
    SuccessState =
        case ocpp_rpc:action(RPCCall) of
            ~"BootNotification" ->
                provisioning;
            _ when State#state.status =:= pending ->
                %% only state with accepted = false and a message other than a
                %% BootNotification
                boot_pending
        end,
    do_connect(Pid, From, Versions, State, SuccessState).

reconnecting({call, From}, {rpc, Pid, RPCBinary}, State) ->
    case decode_rpc(RPCBinary, Pid, State) of
        {ok, {call, Call}} ->
            case ocpp_message:action(ocpp_rpc:payload(Call)) of
                <<"BootNotification">> ->
                    logger:info("reconnected station ~p booting.", [State#state.stationid]),
                    {next_state, connected, State, [postpone]};
                _ ->
                    logger:info("reconnected station ~p continuing.", [State#state.stationid]),
                    {next_state, accepted, State, [postpone]}
            end;
        {error, not_connected} ->
            logger:warning(
                "got RPC from process that is not connected to station ~p~nRPC: ~p",
                [State#state.stationid, RPCBinary]
            ),
            {keep_state_and_data, [{reply, From, {error, not_connected}}]};
        {ok, {callresult, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {ok, {callerror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {ok, {callresulterror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {ok, {send, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {unknown_action, ID}} ->
            logger:info("CALLRESULT for unknown action ~p dropped", [ID]),
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {callerror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {callresulterror, _}} ->
            {keep_state_and_data, [{reply, From, ok}]};
        {error, {error, _}} ->
            {keep_state_and_data, [{reply, From, ok}]}
    end;
reconnecting({call, From}, {connect, _Pid, _Versions}, _State) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]};
reconnecting(
    info,
    {timeout, TRef, {rpccall, MessageID}},
    #state{pending_call = {MessageID, _, _, TRef}} = State
) ->
    logger:info("RPCCALL to station timed out (MessageID = ~p)", [MessageID]),
    {keep_state, State#state{pending_call = undefined}};
reconnecting(info, {timeout, _, {rpccall, _}}, _State) ->
    keep_state_and_data;
reconnecting({call, From}, {disconnect, Pid}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("Station ~p disconnected", [State#state.stationid]),
    demonitor(Ref),
    {next_state, offline, State#state{connection = undefined}, [{reply, From, ok}]};
reconnecting(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info("station ~p disconnected while in reconnecting state.~nReason = ~p", [
        State#state.stationid, Reason
    ]),
    {next_state, offline, State#state{connection = undefined}}.

do_connect(Pid, From, Versions, State, SuccessState) ->
    case ocpp_station_manager:connect(State#state.stationid, Versions) of
        {ok, Version} ->
            logger:info(
                "Connection to station ~p accepted at version ~p (ConnectionPid = ~p)",
                [State#state.stationid, Version, Pid]
            ),
            Ref = erlang:monitor(process, Pid),
            {next_state, SuccessState, State#state{connection = {Version, Pid, Ref}}, [
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
    end.

do_rpc_call(_RPC, #state{connection = undefined}) ->
    {error, not_connected};
do_rpc_call(RPC, #state{pending_call = undefined} = State) ->
    rpcsend(State#state.connection, RPC),
    Message = ocpp_rpc:payload(RPC),
    MessageID = ocpp_rpc:id(RPC),
    {ok, TRef} = ocpp_timer:set_timeout(
        State#state.rpccall_timeout, {rpccall, MessageID}
    ),
    {ok, State#state{pending_call = {MessageID, ocpp_message:action(Message), Message, TRef}}};
do_rpc_call(_RPC, #state{pending_call = {ID, _, _, _}}) ->
    {error, {call_pending, ID}}.

decode_rpc(RPCBinary, Pid, #state{connection = {Version, Pid, _}} = State) ->
    Opts =
        case State#state.pending_call of
            undefined ->
                [];
            {MessageID, Action, _, _} ->
                [{expected, Action}, {id, MessageID}]
        end,
    ocpp_rpc:decode(Version, RPCBinary, Opts);
decode_rpc(RPCBinary, Pid, _) ->
    {error, not_connected}.

pending_call_id(#state{pending_call = {MessageID, _, _, _}}) ->
    MessageID;
pending_call_id(_) ->
    undefined.

handle_callresult(RPC, From, #state{pending_call = undefined}) ->
    logger:info(
        "got CALLRESULT (ID = ~p) but there is no pending call~n"
        "message dropped~n~p",
        [
            ocpp_rpc:id(RPC), ocpp_rpc:payload(RPC)
        ]
    ),
    {keep_state_and_data, [
        {reply, From, ok}
    ]};
handle_callresult(RPC, From, #state{pending_call = {PendingID, _, _PendingCall, TRef}} = State) ->
    Message = ocpp_rpc:payload(RPC),
    case ocpp_rpc:id(RPC) of
        PendingID ->
            ocpp_timer:cancel(TRef),
            process_callresult(Message, From, State);
        ID when ID =/= PendingID ->
            logger:warning(
                "got CALLRESULT for message ID ~p, but ~p is pending~nmessage dropped",
                [ID, PendingID]
            ),
            {keep_state_and_data, [
                {reply, From, ok}
            ]}
    end.

process_callresult(Message, From, #state{pending_call = {ID, _, PendingCall, _}} = State) ->
    PendingAction = ocpp_message:action(PendingCall),
    case ocpp_message:action(Message) of
        PendingAction = ~"TriggerMessage" ->
            {keep_state,
                State#state{
                    pending_call = undefined,
                    triggered = maybe_add_triggered(Message, PendingCall, State#state.triggered)
                },
                [{reply, From, ok}]};
        %% TODO special handling for Report and Get/Set Variables messages
        PendingAction ->
            %% TODO dispatch to handler
            {keep_state, State#state{pending_call = undefined}, [{reply, From, ok}]};
        Action ->
            logger:error(
                "Got response for message ~p; however, action (~p) does not match pending call (~p)",
                [ID, Action, PendingAction]
            ),
            %% TODO dispatch to handler to inform about the error - response is dropped
            {keep_state, State#state{pending_call = undefined}, [{reply, From, ok}]}
    end.

%% Handle CALL messages that arrive while in pending state. This is basically just a
%% pre-filter before passing legal messages to handle_call/2
handle_call_pending(RPC, From, #state{connection = {Version, _, _}} = State) ->
    Message = ocpp_rpc:payload(RPC),
    case ocpp_message:action(Message) of
        <<"BootNotification">> ->
            Payload = ocpp_rpc:payload(RPC),
            ocpp_handler:rpc_call(
                State#state.stationid, ocpp_message:type(Payload), ocpp_rpc:id(RPC), Payload
            ),
            NewState = cancel_pending_call(State),
            NewState1 = clear_trigger(~"BootNotification", Message, NewState),
            {next_state, provisioning, NewState1#state{rpc_call = RPC}, [{reply, From, ok}]};
        Action when is_map_key(Action, State#state.triggered) ->
            handle_call(RPC, From, State);
        _ when Version =:= '1.6' ->
            logger:error(
                "OCPP Error~n"
                "Received disallwed/unsolicited CALL from station while boot pending~n"
                "~p",
                [RPC]
            ),
            %% The 1.6 spec does not require an error response.
            {keep_state_and_data, [{reply, From, ok}]};
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
        _ ->
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

cancel_pending_call(State = #state{pending_call = undefined}) ->
    State;
cancel_pending_call(State = #state{pending_call = {ID, _, _, TRef}}) ->
    logger:info("Pending CALL ~p to station canceled", [ID]),
    ocpp_timer:cancel(TRef),
    State#state{pending_call = undefined}.

rpcsend({_, Conn, _}, RPC) ->
    Conn ! {ocpp, {rpcsend, ocpp_rpc:encode(RPC)}}.

-ifdef(TEST).
%% Set a trace on the station using `m:dbg`.
trace(StationID) ->
    Pid = gproc:whereis_name(?name(StationID)),
    dbg:p(Pid, [m, c]),
    dbg:tp(ocpp_station, x).
-endif.
