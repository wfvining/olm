-module(ocpp_evse).

-behavior(gen_statem).

-export([start_link/0]).
-export([init/1, callback_mode/0]).
%% State callbacks
-export([available/3, unavailable/3, reserved/3, occupied/3, faulted/3]).

-record(state, {}).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

init([]) ->
    {ok, available, #state{}}.

callback_mode() ->
    state_functions.

available(_, _, _) ->
    %% unavailable | reserved | occupied | faulted
    todo.

unavailable(_, _, _) ->
    %% faulted | available | ? is it possible to reserve an unavailable EVSE?
    todo.

reserved(_, _, _) ->
    %% occupied | available | faulted?
    todo.

occupied(_, _, _) ->
    %% faulted | available
    todo.

faulted(_, _, _) ->
    %% available
    todo.
