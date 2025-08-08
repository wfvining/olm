-module(ocpp_report_sup).

-behavior(supervisor).

-export([start_link/1, start_handler/3]).
-export([init/1]).

-export_type([handler_spec/0]).

-type handler_spec() :: {module(), [term()]}.

-define(registry(StationID), {via, gproc, ?name(StationID)}).
-define(name(Name), {n, l, {ocpp_station, Name, report_sup}}).

-spec start_link(StationID :: binary()) -> supervisor:startlink_ret().
start_link(StationID) ->
    supervisor:start_link(?registry(StationID), ?MODULE, StationID).

-spec start_handler(StationID :: binary(), ReportID :: binary(), ExtraHandlers :: [handler_spec()]) ->
    supervisor:startchild_ret().
start_handler(StationID, ReportID, ExtraHandlers) ->
    supervisor:start_child(?registry(StationID), [ReportID, ExtraHandlers]).

init(StationID) ->
    SupFlags = #{
        strategy => simple_one_for_one
    },
    ChildSpec = #{
        id => ocpp_report_handler,
        start => {ocpp_report_handler, start_link, [StationID]}
    },
    {ok, {SupFlags, [ChildSpec]}}.
