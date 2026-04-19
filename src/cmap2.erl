-module(cmap2).
-include_lib("eunit/include/eunit.hrl").

-export([new/2]).

-export_type([mapspec/0]).

-type mapspec() :: #{
    required => [atom()],
    properties := #{atom() => itemspec()},
    additional_properties => boolean(),
    defs => #{atom() => itemspec()}
}.
-type itemspec() :: primitivespec() | {object, mapspec()} | {array, arrayspec()} | {ref, atom()}.
-type primitivespec() ::
    boolean
    | datetime
    | {number, numberspec(float() | integer())}
    | {integer, numberspec(integer())}
    | {float, numberspec(float())}
    | {string, stringspec()}
    | {enum, [atom()]}.
-type numberspec(T) :: #{min => T, max => T}.
-type stringspec() :: #{max_length => non_neg_integer()}.
-type arrayspec() :: #{
    items := itemspec(), min_items => non_neg_integer(), max_items => non_neg_integer()
}.

%% TODO this spec is over-broad in its return type
-spec new(#{atom() | binary() => term()}, mapspec()) -> #{atom() | binary() => term()}.
new(Map, Spec) when is_map(Map), is_map(Spec) ->
    build_value(Map, {object, Spec}, maps:get(defs, Spec, #{}));
new(_, _) ->
    error(badarg).

build_value(Bool, boolean, _) when is_boolean(Bool) ->
    {ok, Bool};
build_value(_, boolean, _) ->
    {error, type_error};
build_value(X, {integer, _}, _) when not is_integer(X) ->
    {error, type_error};
build_value(X, {float, _}, _) when not is_float(X) ->
    {error, type_error};
build_value(X, {number, _}, _) when not is_number(X) ->
    {error, type_error};
build_value(X, {string, _}, _) when not is_binary(X) ->
    {error, type_error};
build_value(X, {string, Constraints}, _) when is_binary(X) ->
    MaxLen = maps:get(max_length, Constraints, inf),
    case unicode:characters_to_binary(X, utf8) of
        {_, _, _} ->
            {error, type_error};
        _ ->
            Len = string:length(X),
            if
                Len =< MaxLen ->
                    {ok, X};
                Len > MaxLen ->
                    {error, value_error}
            end
    end;
build_value(X, {enum, _}, _) when not is_atom(X), not is_binary(X) ->
    {error, type_error};
build_value(X, {Type, Constraints}, _) when
    Type =:= integer, is_integer(X);
    Type =:= float, is_float(X);
    Type =:= number, is_number(X)
->
    Min = maps:get(min, Constraints, inf),
    Max = maps:get(max, Constraints, inf),
    case {Min, Max} of
        {inf, inf} ->
            {ok, X};
        {Min, inf} when X >= Min ->
            {ok, X};
        {inf, Max} when X =< Max ->
            {ok, X};
        {Min, Max} when X >= Min, X =< Max ->
            {ok, X};
        _ ->
            {error, value_error}
    end;
build_value(X, {enum, Values}, Defs) when is_binary(X) ->
    try
        build_value(binary_to_existing_atom(X, utf8), {enum, Values}, Defs)
    catch
        error:badarg ->
            {error, value_error}
    end;
build_value(X, {enum, Values}, _) when is_atom(X) ->
    case lists:member(X, Values) of
        true ->
            {ok, X};
        false ->
            {error, value_error}
    end;
build_value(X, datetime, _) when is_binary(X) ->
    try
        {ok, datetime:from_rfc3339(X)}
    catch
        error:_ ->
            {error, type_error}
    end;
build_value({{_, _, _}, {_, _, _}} = DT, datetime, _) ->
    try
        {ok, datetime:from_local_time(DT)}
    catch
        error:_ ->
            {error, value_error}
    end;
build_value(X, datetime, _) ->
    case datetime:is_datetime(X) of
        true ->
            {ok, X};
        false ->
            {error, type_error}
    end;
build_value(Xs, {array, #{items := ItemConstraint} = Constraints}, Defs) when is_list(Xs) ->
    try
        {Vals, _} = lists:mapfoldl(
            fun(X, N) ->
                case build_value(X, ItemConstraint, Defs) of
                    {ok, Val} ->
                        {Val, N + 1};
                    {error, Reason} ->
                        throw({error, extend_reason(Reason, {element, N}, X, ItemConstraint)})
                end
            end,
            1,
            Xs
        ),
        Min = maps:get(min_items, Constraints, 0),
        Max = maps:get(max_items, Constraints, inf),
        if
            length(Vals) =< Max,
            length(Vals) >= Min ->
                {ok, Vals};
            true ->
                {error, occurrence_error}
        end
    catch
        throw:{error, Reason} ->
            {error, Reason}
    end;
build_value(_, {array, _}, _) ->
    {error, type_error};
build_value(X, {object, #{properties := Properties} = Constraints}, Defs) when is_map(X) ->
    try
        Result = maps:fold(
            fun
                F(K, V, Acc) when is_map_key(K, Properties) ->
                    Spec = maps:get(K, Properties),
                    case build_value(V, Spec, Defs) of
                        {ok, Val} -> Acc#{K => Val};
                        {error, Reason} -> throw({property_error, K, Reason})
                    end;
                F(K, V, Acc) when is_binary(K) ->
                    case maybe_to_atom(K) of
                        Key when is_map_key(Key, Properties) ->
                            try
                                F(Key, V, Acc)
                            catch
                                throw:{property_error, Key, Reason} ->
                                    throw({property_error, K, Reason})
                            end;
                        _ ->
                            %% handle this case specially so the error has the original key and not an atom
                            Acc#{K => V}
                    end;
                F(K, V, Acc) ->
                    %% Pass through all unknown keys. They will be handled later
                    Acc#{K => V}
            end,
            #{},
            X
        ),
        case
            {
                maps:get(required, Constraints, []) -- maps:keys(Result),
                maps:get(additional_properties, Constraints, false),
                maps:keys(Result) -- maps:keys(Properties)
            }
        of
            {[], _, []} ->
                {ok, Result};
            {[], true, _} ->
                {ok, Result};
            {Missing, _, _} when length(Missing) > 0 ->
                {error, {property_error, {missing_properties, Missing}}};
            {_, false, Extra} when length(Extra) > 0 ->
                {error, {property_error, {illegal_properties, Extra}}}
        end
    catch
        throw:{property_error, Key, Reason} ->
            %% An error occured when loading the value for property `Key`
            {error,
                extend_reason(
                    Reason,
                    Key,
                    maps:get(Key, X),
                    maps:get(
                        Key, Properties, maps:get(maybe_to_atom(Key), Properties, '$undefined')
                    )
                )}
    end;
build_value(X, {object, _}, _) when not is_map(X) ->
    {error, type_error};
build_value(X, {ref, Ref}, Defs) ->
    Spec = resolve_ref(Ref, Defs),
    build_value(X, Spec, Defs).

resolve_ref(Ref, Defs) when is_map_key(Ref, Defs) ->
    resolve_ref(maps:get(Ref, Defs), Defs, [Ref]);
resolve_ref(Ref, _) ->
    error({badref, Ref}).

resolve_ref({ref, Ref}, Defs, Path) when is_map_key(Ref, Defs) ->
    case lists:member(Ref, Path) of
        true ->
            error({circularref, lists:reverse(Path)});
        false ->
            resolve_ref(maps:get(Ref, Defs), Defs, [Ref | Path])
    end;
resolve_ref({ref, Ref}, _, _) ->
    error({badref, Ref});
resolve_ref(Spec, _, _) ->
    Spec.

maybe_to_atom(K) when is_atom(K) ->
    K;
maybe_to_atom(K) when is_binary(K) ->
    try
        binary_to_existing_atom(K, utf8)
    catch
        error:_ ->
            K
    end.

extend_reason(
    {property_error, {Reason, #{key := KeyPath, value := _, spec := _} = ErrorDescription}},
    Key,
    _Value,
    _Spec
) ->
    {Reason, ErrorDescription#{key => [Key | KeyPath]}};
extend_reason({property_error, _} = Reason, Key, Value, Spec) ->
    {Reason, #{key => [Key], value => Value, spec => Spec}};
extend_reason(
    {Reason, #{key := KeyPath, value := _, spec := _} = ErrorDescription}, Key, _Value, _Spec
) ->
    {Reason, ErrorDescription#{key => [Key | KeyPath]}};
extend_reason(Reason, Key, Value, Spec) ->
    {Reason, #{key => [Key], value => Value, spec => Spec}}.
