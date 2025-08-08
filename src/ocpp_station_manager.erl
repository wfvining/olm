-module(ocpp_station_manager).

-behaviour(gen_server).

-export([start_link/3, connect/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(registry(StationID), {via, gproc, ?name(StationID)}).
-define(name(StationID), {n, l, {ocpp_station, StationID, manager}}).

-record(state, {stationid :: binary(), principal_handler :: gen_event:handler()}).

start_link(StationID, Handler, Options) ->
    gen_server:start_link(?registry(StationID), ?MODULE, {StationID, Handler, Options}, []).

connect(StationID, Versions) ->
    gen_server:call(?registry(StationID), {connect, Versions}).

init({StationID, Handler, _Options}) ->
    {ok, #state{stationid = StationID, principal_handler = Handler}}.

handle_call({connect, Versions}, _From, State) ->
    ocpp_handler:connect(
        State#state.stationid, State#state.principal_handler, Versions
    );
handle_call(_, _, State) ->
    %% TODO
    {noreply, State}.

handle_cast(_, State) ->
    %% TODO
    {noreply, State}.
