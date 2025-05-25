-module(cmap).
-moduledoc """
Constrained maps.

This module defines functions for constructing and validating maps
with constrained keys and values.
""".

-export([new/2, new/3]).
-export([string/1, string_/1, datetime/1, enum_/1, integer/1, integer_/1, number/1, boolean/1]).

-export_type([spec/0]).

-type spec() :: #{atom => fun()}.
-type string_constraints() :: #{max_length => non_neg_integer()}.
-type integer_constraints() :: #{min => integer(), max => integer()}.

-doc """
Ensure `Value` is a unicode string. Returns the string as a unicode binary.
""".
-spec string(unicode:unicode_binary() | string()) -> unicode:unicode_binary().
string(Bin) when is_binary(Bin) ->
    Bin;
string(Str) when is_list(Str) ->
    Valid = lists:all(fun(X) -> X =< 16#10ffff end, Str),
    if
        Valid ->
            unicode:characters_to_binary(Str);
        true ->
            error({badvalue, {not_unicode, Str}})
    end.

-doc """
Return a constructor for a constrained string.
""".
-spec string_(string_constraints()) ->
    fun((string() | unicode:unicode_binary()) -> unicode:unicode_binary()).
string_(#{max_length := MaxLen}) ->
    fun(Str) ->
        Str1 = string(Str),
        case string:length(Str1) > MaxLen of
            true ->
                error(
                    {badvalue, {string_too_long, [{len, string:length(Str1)}, {limit, {MaxLen}}]}}
                );
            false ->
                Str1
        end
    end.

datetime(Bin) when is_binary(Bin) ->
    datetime(binary_to_list(Bin));
datetime(List) when is_list(List) ->
    try
        T = calendar:rfc3339_to_system_time(List),
        calendar:system_time_to_universal_time(T, second)
    catch
        error:_ ->
            error({badvalue, {invalid_datetime, List}})
    end;
datetime(Datetime = {Date, {H, M, S}}) when H < 24, H >= 0, M < 60, M >= 0, S < 60, S >= 0 ->
    case calendar:valid_date(Date) of
        true ->
            Datetime;
        false ->
            error({badvalue, {invalid_date, Date}})
    end;
datetime({{_, _, _}, Time = {_, _, _}}) ->
    error({badvalue, {invalid_time, Time}}).

-spec enum_(Values :: [atom()]) -> fun((atom() | binary()) -> atom()).
enum_(Values) ->
    fun
        (Val) when is_binary(Val) ->
            try
                V = binary_to_existing_atom(Val),
                case lists:member(V, Values) of
                    true -> V;
                    false -> error({badvalue, {invalid_enum_value, V}})
                end
            catch
                error:_ ->
                    error({badvalue, {invalid_enum_value, Val}})
            end;
        (Val) when is_list(Val) ->
            try
                V = list_to_existing_atom(Val),
                case lists:member(V, Values) of
                    true -> V;
                    false -> error({badvalue, {invalid_enum_value, V}})
                end
            catch
                error:_ ->
                    error({badvalue, {invalid_enum_value, Val}})
            end;
        (Val) when is_atom(Val) ->
            case lists:member(Val, Values) of
                true -> Val;
                false -> error({badvalue, {invalid_enum_value, Val}})
            end
    end.

-spec integer(integer()) -> integer().
integer(I) when is_integer(I) ->
    I;
integer(I) ->
    error({badvalue, {not_an_integer, I}}).

-spec integer_(integer_constraints()) -> fun((integer()) -> integer()).
integer_(Constraints) ->
    fun(I) ->
        Sat = maps:fold(
            fun
                (min, Min, Acc) ->
                    Acc andalso I >= Min;
                (max, Max, Acc) ->
                    Acc andalso I =< Max
            end,
            true,
            Constraints
        ),
        if
            Sat -> I;
            not Sat -> error({badvalue, {invalid, I}})
        end
    end.

-spec number(Num :: number()) -> number().
number(Num) when is_number(Num) ->
    Num;
number(NotNumber) ->
    error({badvalue, {not_number, NotNumber}}).

-spec boolean(Bool :: boolean()) -> boolean().
boolean(Bool) when is_boolean(Bool) ->
    Bool;
boolean(NotBool) ->
    error({badvalue, {not_boolean, NotBool}}).

-doc #{equiv => new(Spec, Map, [])}.
-spec new(spec(), #{atom() | binary() | string() => term()}) -> #{atom() | binary() => term()}.
new(Spec, Map) ->
    new(Spec, Map, []).

-spec new(
    spec(),
    #{atom() | binary() | string() => term()},
    Options :: [Option]
) -> #{atom() | binary() => term()} when
    Option :: {required, [atom()]} | {extra_keys, boolean()} | extra_keys.
new(Spec, Map, Options) ->
    Required = proplists:get_value(required, Options, []),
    maybe
        true ?= lists:all(fun(K) -> lists:member(K, maps:keys(Spec)) end, Required),
        M = maps:fold(
            fun(K, V, Acc) ->
                Key = to_key(K, maps:keys(Spec)),
                case proplists:get_bool(extra_keys, Options) of
                    false when is_binary(Key) ->
                        error(badarg);
                    false when is_atom(Key) ->
                        Constructor = maps:get(Key, Spec),
                        Acc#{Key => Constructor(V)};
                    true ->
                        Constructor = maps:get(Key, Spec, fun(X) -> X end),
                        Acc#{Key => Constructor(V)}
                end
            end,
            #{},
            Map
        ),
        [] ?= Required -- maps:keys(M),
        M
    else
        false ->
            error(badarg);
        Missing when is_list(Missing), length(Missing) > 0 ->
            error({missing_keys, Missing})
    end.

to_key(K, Keys) when is_atom(K) ->
    case lists:member(K, Keys) of
        true ->
            K;
        false ->
            atom_to_binary(K)
    end;
to_key(K, Keys) when is_binary(K) ->
    try
        to_key(binary_to_existing_atom(K, utf8), Keys)
    catch
        error:badarg ->
            K
    end;
to_key(K, Keys) when is_list(K) ->
    try
        to_key(list_to_existing_atom(K), Keys)
    catch
        error:badarg ->
            unicode:characters_to_binary(K)
    end.
