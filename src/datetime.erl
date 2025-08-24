-module(datetime).
-moduledoc """
This module provides utilities for working with date-times. These
utilities enable storing more information that can be represented by
the standard `{{Year, Month, Day}, {Hour, Minute, Second}}` structure.
Most importantly information on the precision of a timestamp is
included in the datetime structures returned by this module.
""".

-export([from_rfc3339/1, to_rfc3339/1, from_local_time/1]).
-export([is_datetime/1]).

-export_type([datetime/0]).

-record(datetime, {gregorianseconds :: integer(), milliseconds = 0 :: integer()}).

-opaque datetime() :: #datetime{}.

-spec from_rfc3339(binary()) -> datetime().
from_rfc3339(Bin) ->
    SystemTime = calendar:rfc3339_to_system_time(binary_to_list(Bin), [{unit, millisecond}]),
    Millis = SystemTime rem 1000,
    UniversalTime = calendar:system_time_to_universal_time(SystemTime, millisecond),
    GregorianSeconds = calendar:datetime_to_gregorian_seconds(UniversalTime),
    #datetime{gregorianseconds = GregorianSeconds, milliseconds = Millis}.

-spec to_rfc3339(datetime()) -> binary().
to_rfc3339(#datetime{gregorianseconds = GregorianSeconds, milliseconds = Millis}) ->
    Milliseconds = GregorianSeconds * 1000 + Millis,
    Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) * 1000,
    SystemTimeUTC = Milliseconds - Epoch,
    unicode:characters_to_binary(
        calendar:system_time_to_rfc3339(
            SystemTimeUTC, [{unit, millisecond}, {offset, "Z"}]
        )
    ).

-spec from_local_time(calendar:datetime()) -> datetime().
from_local_time(DateTime) ->
    %% Let the OS guess about DST (rather than calendar:local_time_to_universal_time_dst/1)
    DateTimeUTC = erlang:localtime_to_universaltime(DateTime),
    #datetime{gregorianseconds = calendar:datetime_to_gregorian_seconds(DateTimeUTC)}.

-spec is_datetime(term()) -> boolean().
is_datetime(#datetime{}) ->
    true;
is_datetime(_) ->
    false.
