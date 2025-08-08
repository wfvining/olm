-module(ocpp_station_supsup).

-behaviour(supervisor).

-export([start_link/0, add_station/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_station(StationID) ->
    supervisor:start_child(?SERVER, [StationID]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 1
    },
    ChildSpec = #{
        id => ocpp_station_sup,
        start => {ocpp_station_sup, start_link, []},
        type => supervisor
    },
    {ok, {SupFlags, [ChildSpec]}}.
