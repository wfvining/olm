-module(prop_ocpp201_fsm).
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
            fun() ->
                meck:unload(ocpp_station_manager),
                lists:foreach(fun application:stop/1, Apps)
            end
        end,
        ?FORALL(
            Cmds,
            proper_fsm:commands(?MODULE),
            begin
                {ok, _Pid} = ocpp_station:start_link(?STATIONID),
                {History, State, Result} = proper_fsm:run_commands(?MODULE, Cmds),
                ocpp_station:stop(?STATIONID),
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
                        Result =:= ok
                    )
                )
            end
        )
    ).

-record(data, {
    %% a call in progress from the station to the csms
    station_cip :: ocpp_rpc:call() | undefined,
    %% a call in progress from the csms to the station

    %% not sure this will be the right type
    csms_cip :: ocpp_rpc:call() | undefined,
    %% list of TriggerMessageRequests
    accepted_triggers = [] :: [ocpp_message:message()],
    rejected_triggers = [] :: [ocpp_message:message()]
}).

shuffle(Xs) ->
    [X || {X, _} <- lists:keysort(2, [{X, rand:uniform()} || X <- Xs])].

%% Initial state for the state machine
initial_state() -> {offline, not_booted}.
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

connected(not_booted, _Data) ->
    [
        {booting,
            {call, station201_shim, boot, [
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
            {call, station201_shim, station_rpccall_not_booted, [
                ?STATIONID,
                ?SUCHTHAT(
                    Message,
                    ocpp_message_gen:request('2.0.1'),
                    ocpp_message:action(Message) =/= ~"BootNotification"
                )
            ]}},
        {history,
            {call, station201_shim, csms_rpccall_not_booted, [
                ?STATIONID,
                ocpp_message_gen:request('2.0.1')
            ]}}
    ];
connected(accepted, _Data) ->
    %% booted and accepted
    [{history, {call, ?MODULE, todo, ["allow heartbeat and other messages, allow boot, etc..."]}}];
connected(booted, _Data) ->
    %% booted but not accepted
    [{history, {call, ?MODULE, todo, ["Heartbeat fails"]}}].

booting(#data{station_cip = BootCall}) ->
    [
        {idle,
            {call, station201_shim, accept_boot, [
                ?STATIONID,
                BootCall,
                ocpp_message_gen:message(
                    '2.0.1',
                    ~"BootNotificationResponse",
                    [{override, #{status => 'Accepted'}}]
                )
            ]}},
        {pending,
            {call, station201_shim, pending_boot, [
                ?STATIONID,
                BootCall,
                ocpp_message_gen:message(
                    '2.0.1',
                    ~"BootNotificationResponse",
                    [{override, #{status => 'Pending'}}]
                )
            ]}},
        {
            {connected, booted},
            {call, station201_shim, reject_boot, [
                ?STATIONID,
                BootCall,
                ocpp_message_gen:message('2.0.1', ~"BootNotificationResponse", [
                    {override, #{status => 'Rejected'}}
                ])
            ]}
        }
    ].

pending(_Data) ->
    %% received a BootNotificationResponse with status=Pending
    [{history, {call, ?MODULE, todo, ["Configuration Messages"]}}].

todo(What) ->
    io:format("WARNING - TODO (~s)\n", [What]),
    todo.

idle(_Data) ->
    %% Station has booted and is ready for normal operation
    [
        {
            {idle, cip},
            {call, station201_shim, heartbeat, [
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
    Message = ocpp_rpc:payload(PendingCall),
    Action = ocpp_message:action(Message),
    MessageID = ocpp_rpc:id(PendingCall),
    [
        {idle,
            {call, station201_shim, valid_reply, [
                ?STATIONID,
                MessageID,
                ocpp_message_gen:message('2.0.1', <<Action/binary, "Response">>)
            ]}}
    ].

%% Optional callback, weight modification of transitions
weight({offline, _}, {connected, _}, _Call) -> 10;
weight(_FromState, _ToState, _Call) -> 1.

%% Picks whether a command should be valid.
precondition(_From, _To, #data{}, {call, _Mod, _Fun, _Args}) -> true.

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
    {call, station201_shim, boot, _},
    ok
) ->
    true;
postcondition(
    {connected, not_booted},
    {connected, not_booted},
    _Data,
    {call, station201_shim, station_rpccall_not_booted, _},
    {error, not_provisioned}
) ->
    true;
postcondition(
  {connected, not_booted},
  {connected, not_booted},
  _Data,
  {call, station201_shim, csms_rpccall_not_booted, [_, Message]},
  {error, not_provisioned}
 ) ->
    Action = ocpp_message:action(Message),
    timeout =:= station201_shim:recv(Action);
postcondition(
    booting,
    idle,
    _Data,
    {call, station201_shim, accept_boot, [_, _, Response]},
    {ok, {callresult, RPCResult}}
) ->
    Message = ocpp_rpc:payload(RPCResult),
    ocpp_message:type(Message) =:= ~"BootNotificationResponse" andalso
        ocpp_message:get(status, Message) =:= 'Accepted' andalso
        ocpp_message:get(currentTime, Message) =:= ocpp_message:get(currentTime, Response) andalso
        ocpp_message:get(interval, Message) =:= ocpp_message:get(interval, Response);
postcondition(
    booting,
    pending,
    _Data,
    {call, station201_shim, pending_boot, [_, _, Response]},
    {ok, {callresult, RPCResult}}
) ->
    Message = ocpp_rpc:payload(RPCResult),
    ocpp_message:type(Message) =:= ~"BootNotificationResponse" andalso
        ocpp_message:get(status, Message) =:= 'Pending' andalso
        ocpp_message:get(currentTime, Message) =:= ocpp_message:get(currentTime, Response) andalso
        ocpp_message:get(interval, Message) =:= ocpp_message:get(interval, Response);
postcondition(
    booting,
    {connected, booted},
    _Data,
    {call, station201_shim, reject_boot, [_, _, Response]},
    {ok, {callresult, RPCResult}}
) ->
    Message = ocpp_rpc:payload(RPCResult),
    ocpp_message:type(Message) =:= ~"BootNotificationResponse" andalso
        ocpp_message:get(status, Message) =:= 'Rejected' andalso
        ocpp_message:get(currentTime, Message) =:= ocpp_message:get(currentTime, Response) andalso
        ocpp_message:get(interval, Message) =:= ocpp_message:get(interval, Response);
postcondition(idle, {idle, cip}, _Data, {call, station201_shim, heartbeat, _}, ok) ->
    true;
postcondition(
    {idle, cip},
    idle,
    #data{station_cip = RPCCall},
    {call, station201_shim, valid_reply, [_, _, Payload]},
    {ok, {callresult, RPCResult}}
) ->
    Message = ocpp_rpc:payload(RPCResult),
    ocpp_message:action(Message) =:= ocpp_message:action(ocpp_rpc:payload(RPCCall)) andalso
        ocpp_rpc:id(RPCResult) =:= ocpp_rpc:id(RPCCall) andalso
        Message =:= Payload;
postcondition(_From, _To, _Data, {call, ?MODULE, todo, _}, todo) ->
    true;
postcondition(_From, _To, _Data, {call, _Mod, _Fun, _Args}, _Res) ->
    false.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state_data(idle, {idle, cip}, Data, _Res, {call, station201_shim, heartbeat, [_, RPCCall]}) ->
    Data#data{station_cip = RPCCall};
next_state_data(booting, idle, Data, _Res, {call, station201_shim, Fun, _}) when
    Fun =:= accept_boot; Fun =:= pending_boot
->
    Data#data{station_cip = undefined};
next_state_data({connected, _}, booting, Data, _Res, {call, station201_shim, boot, [_, RPCCall]}) ->
    Data#data{station_cip = RPCCall};
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    NewData = Data,
    NewData.

%% Helper to generate unique message IDs
messageid() ->
    ?LAZY(integer_to_binary(erlang:unique_integer([positive]), 36)).
