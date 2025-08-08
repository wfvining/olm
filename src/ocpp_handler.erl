-module(ocpp_handler).

-export([start_link/1, connect/3]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {ocpp_station, Name, handler}}).

-callback connect(Versions :: [ocpp:version()]) -> {ok, ocpp:version()} | {error, not_supported}.

connect(StationID, Handler, Versions) ->
    gen_event:call(?registry(StationID), Handler, {connect, Versions}).

start_link(StationID) ->
    gen_event:start_link(?registry(StationID)).
