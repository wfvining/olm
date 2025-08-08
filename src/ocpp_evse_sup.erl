-module(ocpp_evse_sup).

-behavior(supervisor).

-export([start_link/1, new_evse/2]).
-export([init/1]).

-define(registry(StationID), {via, gproc, ?name(StationID)}).
-define(name(Name), {n, l, {ocpp_station, Name, evse_sup}}).

-spec start_link(StationID :: binary()) -> supervisor:startlink_ret().
start_link(StationID) ->
    supervisor:start_link(?registry(StationID), ?MODULE, StationID).

-spec new_evse(StationID :: binary(), EVSEID :: pos_integer()) -> supervisor:startchild_ret().
new_evse(StationID, EVSEID) ->
    supervisor:start_child(?registry(StationID), [EVSEID]).

init(StationID) ->
    SupFlags = #{
        strategy => simple_one_for_one
    },
    ChildSpec = #{
        id => ocpp_report_handler,
        start => {ocpp_report_handler, start_link, [StationID]}
    },
    {ok, {SupFlags, [ChildSpec]}}.
