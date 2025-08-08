-module(ocpp_station_sup).

-behavior(supervisor).

-export([start_link/1, start_link/2]).
-export([init/1]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {ocpp_station, Name, sup}}).

start_link(StationID) ->
    start_link(StationID, []).

start_link(StationID, Options) ->
    supervisor:start_link(?registry(StationID), ?MODULE, {StationID, Options}).

init({StationID, Options}) ->
    SupFlags = #{
        strategy => one_for_one
    },
    DeviceModel = proplists:get_value(device_model, Options, []),
    ChildSpecs = [
        #{
            id => ocpp_station_manager,
            start => {ocpp_station_manager, start_link, [StationID, DeviceModel]}
        },
        #{
            id => ocpp_handler,
            start => {ocpp_handler, start_link, [StationID]}
        },
        #{
            id => ocpp_report_sup,
            start => {ocpp_report_sup, start_link, [StationID]},
            type => supervisor
        },
        #{
            id => ocpp_transaction_sup,
            start => {ocpp_transaction_sup, start_link, [StationID]},
            type => supervisor
        },
        #{
            id => ocpp_evse_sup,
            start => {ocpp_evse_sup, start_link, [StationID]},
            type => supervisor
        },
        #{
            id => ocpp_station,
            start => {ocpp_station, start_link, [StationID]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
