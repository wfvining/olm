-module(ocpp_transaction_sup).

-behavior(supervisor).

-export([start_link/1, start_transaction/2]).
-export([init/1]).

-define(registry(StationID), {via, gproc, ?name(StationID)}).
-define(name(Name), {n, l, {ocpp_station, Name, transaction_sup}}).

start_link(StationID) ->
    supervisor:start_link(?registry(StationID), ?MODULE, StationID).

start_transaction(StationID, TransactionID) ->
    %% TODO probably need more than the tx ID to set this up properly
    supervisor:start_child(?registry(StationID), TransactionID).

init(StationID) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{
        id => ocpp_transaction_handler,
        start => {ocpp_transaction, start_link, [StationID]}
    },
    {ok, {SupFlags, [ChildSpec]}}.
