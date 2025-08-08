-module(ocpp_manager).

-behaviour(gen_server).

-export([start_link/0, add_station/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_station(StationID :: binary()) -> {ok, pid()} | {error, Reason :: term()}.
add_station(StationID) ->
    gen_server:call(?SERVER, {add_station, StationID}).

init([]) ->
    {ok, #state{}}.

handle_call({add_station, StationID}, _From, State) ->
    Result = ocpp_station_supsup:add_station(StationID),
    {reply, Result, State};
handle_call(_, _, State) ->
    %% TODO
    {noreply, State}.

handle_cast(_, State) ->
    %% TODO
    {noreply, State}.
