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

-export([start_link/1, start_link/2, stop/1, connect/2]).
-export([init/1, callback_mode/0]).
%% State callbacks
-export([
    unprovisioned/3,
    connected/3,
    provisioning/3,
    boot_pending/3,
    provisioning_call_pending/3,
    accepted/3,
    call_pending/3,
    offline/3,
    reconnecting/3
]).

-define(registry(StationID), {via, gproc, ?name(StationID)}).
-define(name(Name), {n, l, {ocpp_station, Name, station}}).

-record(state, {
    stationid :: binary(),
    connection ::
        {pending, gen_statem:from(), pid()} | {ocpp:version(), pid(), reference()} | undefined
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
    end.

connected(info, {'DOWN', Ref, process, Pid, Reason}, #state{connection = {_, Pid, Ref}} = State) ->
    logger:info(
        "Connection down before provisioning. ConnectionPid=~p, Reason=~p",
        [Pid, Reason]
    ),
    {next_state, unprovisioned, State#state{connection = undefined}};
connected({call, From}, {connect, _, _}, _State) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]}.

provisioning(_, _, _) ->
    todo.

boot_pending(_, _, _) ->
    todo.

provisioning_call_pending(_, _, _) ->
    todo.

accepted(_, _, _) ->
    todo.

call_pending(_, _, _) ->
    todo.

offline(_, _, _) ->
    todo.

reconnecting(_, _, _) ->
    todo.
