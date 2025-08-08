-module(ocpp_transaction).

-behavior(gen_statem).

-export([start_link/1, start_link/2]).
-export([init/1, callback_mode/0]).
%% State callbacks
-export([idle/3, connected/3, charging/3, suspended_ev/3, suspended_evse/3]).

-record(state, {id :: binary(), seq :: non_neg_integer()}).

start_link(TransactionID) ->
    start_link(TransactionID, []).

start_link(TransactionID, Options) ->
    gen_statem:start_link(?MODULE, {TransactionID, Options}, []).

callback_mode() ->
    state_functions.

init({TransactionID, Options}) ->
    InitState = proplists:get_value(charging_state, Options, idle),
    Seq = proplists:get_value(sequence_number, Options, 0),
    {ok, InitState, #state{id = TransactionID, seq = Seq}}.

idle(_, _, _) ->
    %% connected
    todo.

connected(_, _, _) ->
    %% charging | suspended_ev | suspended_evse | idle?
    todo.

charging(_, _, _) ->
    %% connected | suspended_ev | suspended_evse
    todo.

suspended_ev(_, _, _) ->
    %% charging | connected | suspended_evse
    todo.

suspended_evse(_, _, _) ->
    %% charging | connected | suspended_ev
    todo.
