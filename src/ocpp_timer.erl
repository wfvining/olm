-module(ocpp_timer).
-moduledoc """
Wrapper around timer functions using `erlang:start_timer/3`.
""".

-export([set_timeout/2, cancel/1]).
-export_type([timer/0]).

-opaque timer() :: reference().

-doc """
Schedule a `Msg` to be delivered to the calling process after `Time`
milliseconds. The message will be delivered as
`{timeout, TimerRef :: reference(), Msg}` where `TimerRef` is the timer
reference returned by `set_timeout/2`.
""".
-spec set_timeout(pos_integer(), term()) -> {ok, timer()}.
set_timeout(Time, Msg) ->
    {ok, erlang:start_timer(Time, self(), Msg)}.

-spec cancel(timer()) -> ok.
cancel(TRef) ->
    erlang:cancel_timer(TRef),
    ok.
