-module(prop_cmap).
-eqwalizer(ignore).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_boolean_values() ->
    ?FORALL(
        {_, X, Valid},
        constrained_boolean(),
        begin
            Spec = #{properties => #{key => boolean}},
            Res =
                if
                    Valid ->
                        {ok, #{key => X}} =:= cmap:new(#{key => X}, Spec);
                    Valid =:= badtype ->
                        make_error(type_error, [key], X, boolean) =:=
                            cmap:new(#{key => X}, Spec)
                end,
            collect(Valid, Res)
        end
    ).

prop_integer_value() ->
    ?FORALL(
        {Constraint, I, Valid},
        frequency([
            {9, constrained_integer()},
            {1,
                ?LET(
                    {Constraint, _, _},
                    constrained_integer(),
                    {Constraint, ?SUCHTHAT(X, term(), not is_integer(X)), badtype}
                )}
        ]),
        begin
            Spec = #{key => {integer, Constraint}},
            OSpec = #{properties => Spec},
            Res =
                if
                    Valid =:= badtype ->
                        make_error(type_error, [key], I, {integer, Constraint}) =:=
                            cmap:new(#{key => I}, OSpec);
                    Valid ->
                        {ok, #{key => I}} =:= cmap:new(#{key => I}, OSpec);
                    not Valid ->
                        make_error(value_error, [key], I, {integer, Constraint}) =:=
                            cmap:new(#{key => I}, OSpec)
                end,
            collect(describe_number({Constraint, I, Valid}), Res)
        end
    ).

prop_float_value() ->
    ?FORALL(
        {Constraint, I, Valid},
        frequency([
            {9, constrained_float()},
            {1,
                ?LET(
                    {Constraint, _, _},
                    constrained_float(),
                    {Constraint, ?SUCHTHAT(X, term(), not is_float(X)), badtype}
                )}
        ]),
        begin
            OSpec = #{properties => #{key => {float, Constraint}}},
            Res =
                if
                    Valid =:= badtype ->
                        make_error(type_error, [key], I, {float, Constraint}) =:=
                            cmap:new(#{key => I}, OSpec);
                    Valid ->
                        {ok, #{key => I}} =:= cmap:new(#{key => I}, OSpec);
                    not Valid ->
                        make_error(value_error, [key], I, {float, Constraint}) =:=
                            cmap:new(#{key => I}, OSpec)
                end,
            collect(describe_number({Constraint, I, Valid}), Res)
        end
    ).

prop_number_value() ->
    ?FORALL(
        {Constraint, I, Valid},
        frequency([
            {9, constrained_number()},
            {1,
                ?LET(
                    {Constraint, _, _},
                    constrained_number(),
                    {Constraint, ?SUCHTHAT(X, term(), not is_number(X)), badtype}
                )}
        ]),
        begin
            OSpec = #{properties => #{key => {number, Constraint}}},
            Res =
                if
                    Valid =:= badtype ->
                        make_error(type_error, [key], I, {number, Constraint}) =:=
                            cmap:new(#{key => I}, OSpec);
                    Valid ->
                        {ok, #{key => I}} =:= cmap:new(#{key => I}, OSpec);
                    not Valid ->
                        make_error(value_error, [key], I, {number, Constraint}) =:=
                            cmap:new(#{key => I}, OSpec)
                end,
            collect(describe_number({Constraint, I, Valid}), Res)
        end
    ).

prop_string_value() ->
    ?FORALL(
        {Constraint, Str, Valid},
        frequency([
            {9, constrained_string()},
            {1,
                ?LET(
                    {Constraint, _, _},
                    constrained_string(),
                    {Constraint,
                        ?SUCHTHAT(
                            X,
                            term(),
                            not is_binary(X) orelse
                                not_unicode(X)
                        ),
                        badtype}
                )}
        ]),
        begin
            OSpec = #{properties => #{key => {string, Constraint}}},
            Res =
                if
                    Valid =:= badtype ->
                        make_error(type_error, [key], Str, {string, Constraint}) =:=
                            cmap:new(#{key => Str}, OSpec);
                    Valid ->
                        {ok, #{key => Str}} =:= cmap:new(#{key => Str}, OSpec);
                    not Valid ->
                        make_error(value_error, [key], Str, {string, Constraint}) =:=
                            cmap:new(#{key => Str}, OSpec)
                end,
            collect(Valid, Res)
        end
    ).

prop_enum_value() ->
    ?FORALL(
        {Values, Value, Valid},
        constrained_enum(),
        begin
            OSpec = #{properties => #{key => {enum, Values}}},
            Res =
                if
                    Valid =:= badtype ->
                        make_error(type_error, [key], Value, {enum, Values}) =:=
                            cmap:new(#{key => Value}, OSpec);
                    Valid, is_binary(Value) ->
                        {ok, #{key => binary_to_existing_atom(Value)}} =:=
                            cmap:new(#{key => Value}, OSpec);
                    Valid, is_atom(Value) ->
                        {ok, #{key => Value}} =:= cmap:new(#{key => Value}, OSpec);
                    not Valid ->
                        make_error(value_error, [key], Value, {enum, Values}) =:=
                            cmap:new(#{key => Value}, OSpec)
                end
        end
    ).

prop_datetime_value() ->
    ?FORALL(
        {_, DateTime, Valid},
        constrained_datetime(),
        begin
            OSpec = #{properties => #{key => datetime}},
            Res =
                if
                    Valid =:= badtype ->
                        make_error(type_error, [key], DateTime, datetime) =:=
                            cmap:new(#{key => DateTime}, OSpec);
                    Valid ->
                        {ok, #{key => load_value(DateTime, datetime)}} =:=
                            cmap:new(#{key => DateTime}, OSpec);
                    not Valid ->
                        make_error(value_error, [key], DateTime, datetime) =:=
                            cmap:new(#{key => DateTime}, OSpec)
                end,
            collect(dttype(DateTime, Valid), Res)
        end
    ).

dttype({{{_, _, _}, {_, _, _}}, _}, Valid) ->
    {localtime, Valid};
dttype(DT, Valid) when is_binary(DT) ->
    {rfc3339, Valid};
dttype(DT, Valid) ->
    case datetime:is_datetime(DT) of
        true ->
            {datetime, Valid};
        false when not is_binary(DT) ->
            {term, Valid};
        false when is_binary(DT) ->
            {rfc3339, Valid}
    end.

prop_array() ->
    ?FORALL(
        {Constraints, Array, Valid},
        constrained_array(),
        begin
            OSpec = #{properties => #{key => {array, Constraints}}},
            Result =
                case Valid of
                    badtype ->
                        make_error(type_error, [key], Array, {array, Constraints}) =:=
                            cmap:new(#{key => Array}, OSpec);
                    {badtype, ErrorPath} ->
                        make_error(
                            type_error,
                            [key | ErrorPath],
                            resolve_path(ErrorPath, Array),
                            resolve_constraint(ErrorPath, {array, Constraints})
                        ) =:=
                            cmap:new(#{key => Array}, OSpec);
                    {false, ErrorPath} ->
                        make_error(
                            value_error,
                            [key | ErrorPath],
                            resolve_path(ErrorPath, Array),
                            resolve_constraint(ErrorPath, {array, Constraints})
                        ) =:=
                            cmap:new(#{key => Array}, OSpec);
                    PossibleErrors when is_list(PossibleErrors) ->
                        Res = cmap:new(#{key => Array}, OSpec),
                        lists:any(
                            fun
                                ({Error, ErrorPath}) ->
                                    make_error(
                                        Error,
                                        [key | ErrorPath],
                                        resolve_path(ErrorPath, Array),
                                        resolve_constraint(ErrorPath, {array, Constraints})
                                    ) =:= Res;
                                (Error) ->
                                    make_error(Error, [key], Array, {array, Constraints}) =:= Res
                            end,
                            PossibleErrors
                        );
                    badlength ->
                        make_error(occurrence_error, [key], Array, {array, Constraints}) =:=
                            cmap:new(#{key => Array}, OSpec);
                    {badlength, ErrorPath} ->
                        make_error(
                            badlength,
                            [key | ErrorPath],
                            resolve_path(ErrorPath, Array),
                            resolve_constraint(ErrorPath, {array, Constraints})
                        ) =:=
                            cmap:new(#{key => Array}, OSpec);
                    true ->
                        ExpectedArray = [
                            load_value(I, maps:get(items, Constraints, undefined))
                         || I <- Array
                        ],
                        {ok, #{key => ExpectedArray}} =:=
                            cmap:new(#{key => Array}, OSpec);
                    false ->
                        make_error(value_error, [key], Array, {array, Constraints}) =:=
                            cmap:new(#{key => Array}, OSpec);
                    {OtherError, ErrorPath} ->
                        make_error(
                            OtherError,
                            [key | ErrorPath],
                            resolve_path(ErrorPath, Array),
                            resolve_constraint(ErrorPath, {array, Constraints})
                        ) =:=
                            cmap:new(#{key => Array}, OSpec)
                end,
            collect(describe_array(Constraints, Valid), Result)
        end
    ).

prop_object() ->
    ?FORALL(
        {Constraints, Object, Valid},
        constrained_object(),
        begin
            OSpec = #{properties => #{key => {object, Constraints}}},
            case Valid of
                badtype ->
                    collect(
                        badtype,
                        make_error(type_error, [key], Object, {object, Constraints}) =:=
                            cmap:new(#{key => Object}, OSpec)
                    );
                {illegal_properties, Keys} ->
                    collect(
                        illegal_properties,
                        {error, {property_error, {illegal_properties, Keys}}} =:=
                            %% Use constraints directly to get top level error instead of a type error
                            cmap:new(Object, Constraints)
                    );
                {missing_properties, Missing} ->
                    collect(
                        missing_properties,
                        case cmap:new(Object, Constraints) of
                            {error, {property_error, {missing_properties, GotMissing}}} ->
                                lists:sort(Missing) =:= lists:sort(GotMissing);
                            _ ->
                                false
                        end
                    );
                {badtype, ErrorPath} ->
                    collect(
                        {badtype, length(ErrorPath)},
                        make_error(
                            type_error,
                            [key | ErrorPath],
                            resolve_path(ErrorPath, Object),
                            resolve_constraint(ErrorPath, {object, Constraints})
                        ) =:=
                            cmap:new(#{key => Object}, OSpec)
                    );
                {badlength, ErrorPath} ->
                    collect(
                        {occurrence_error, length(ErrorPath)},
                        make_error(
                            badlength,
                            [key | ErrorPath],
                            resolve_path(ErrorPath, Object),
                            resolve_constraint(ErrorPath, {object, Constraints})
                        ) =:=
                            cmap:new(#{key => Object}, OSpec)
                    );
                {false, ErrorPath} ->
                    collect(
                        {value_error, length(ErrorPath)},
                        make_error(
                            false,
                            ErrorPath,
                            resolve_path(ErrorPath, Object),
                            resolve_constraint(ErrorPath, {object, Constraints})
                        ) =:=
                            cmap:new(Object, Constraints)
                    );
                {Error, ErrorPath} when is_list(ErrorPath) ->
                    collect(
                        bad_subitem,
                        make_error(
                            Error,
                            [key | ErrorPath],
                            resolve_path(ErrorPath, Object),
                            resolve_constraint(ErrorPath, {object, Constraints})
                        ) =:=
                            cmap:new(#{key => Object}, OSpec)
                    );
                Errors when is_list(Errors) ->
                    Res = cmap:new(#{key => Object}, OSpec),
                    collect(
                        multierror,
                        lists:any(
                            fun
                                ({ErrorType, ErrorPath}) when is_list(ErrorPath) ->
                                    make_error(
                                        ErrorType,
                                        [key | ErrorPath],
                                        resolve_path(ErrorPath, Object),
                                        resolve_constraint(ErrorPath, {object, Constraints})
                                    ) =:= Res;
                                (Error) ->
                                    make_error(Error, [key], Object, Constraints) =:= Res
                            end,
                            Errors
                        )
                    );
                true ->
                    collect(
                        valid,
                        {ok, #{key => load_value(Object, {object, Constraints})}} =:=
                            cmap:new(#{key => Object}, OSpec)
                    )
            end
        end
    ).

prop_to_json() ->
    ?FORALL(
        {Obj, Constraint},
        ?LET(
            Constraint,
            object_constraint(3),
            ?LET(
                Obj,
                valid_value({object, Constraint}),
                {Obj, Constraint}
            )
        ),
        begin
            {ok, Res} = cmap:new(Obj, Constraint),
            JSON = iolist_to_binary(cmap:to_json(Res)),
            {ok, Res1} = cmap:new(json:decode(JSON), Constraint),
            ?WHENFAIL(
                io:format("JSON:~n~s~nRES:~n~p~n", [JSON, Res1]),
                Res =/= JSON andalso
                    only_defined(Res, {object, Constraint}) =:=
                        only_defined(Res1, {object, Constraint})
            )
        end
    ).

only_defined(X, {object, #{properties := Properties}}) ->
    #{
        K => only_defined(V, maps:get(K, Properties))
     || K := V <- maps:with(maps:keys(Properties), X)
    };
only_defined(Xs, {array, #{items := ItemSpec}}) ->
    [only_defined(X, ItemSpec) || X <- Xs];
only_defined(X, _) ->
    X.

describe_array(Constraints, true) ->
    {valid, expected_depth(Constraints)};
describe_array(Constraints, false) ->
    value_error;
describe_array(Constraints, {false, ErrorPath}) ->
    {value_error, expected_depth(Constraints), length(ErrorPath)};
describe_array(Constraints, {Reason, ErrorPath}) ->
    {Reason, expected_depth(Constraints), length(ErrorPath)};
describe_array(Constraints, Reason) when is_list(Reason) ->
    multierror;
describe_array(_, Reason) ->
    Reason.

expected_depth(C) ->
    expected_depth(C, 1).

expected_depth(#{items := {array, C}}, D) ->
    expected_depth(C, D + 1);
expected_depth(_, D) ->
    D.

make_error(ErrorType, ErrorPath, Value, Constraint) ->
    {error, {error_type(ErrorType), #{key => ErrorPath, value => Value, spec => Constraint}}}.

make_error({ErrorType, ErrorPath}, Obj, Constraints) ->
    {error,
        {error_type(ErrorType), #{
            key =>
                if
                    length(ErrorPath) == 1 -> hd(ErrorPath);
                    true -> ErrorPath
                end,
            value => resolve_path(ErrorPath, Obj),
            spec => resolve_constraint(ErrorPath, {object, Constraints})
        }}}.

make_array_error({ErrorType, ErrorPath}, Key, Array, Constraints) ->
    {error,
        {error_type(ErrorType), #{
            key => [Key | ErrorPath],
            value => resolve_path(ErrorPath, Array),
            spec => resolve_constraint(ErrorPath, {array, Constraints})
        }}};
make_array_error(ErrorType, Key, Array, Constraints) ->
    {error, {error_type(ErrorType), #{key => Key, value => Array, spec => {array, Constraints}}}}.

error_type(badtype) ->
    type_error;
error_type(type_error) ->
    type_error;
error_type(badlength) ->
    occurrence_error;
error_type(occurrence_error) ->
    occurrence_error;
error_type({ET, _} = Error) when ET =:= illegal_properties; ET =:= missing_properties ->
    {property_error, Error};
error_type(ET) when ET =:= false; ET =:= value_error ->
    value_error.

resolve_constraint([P], {object, #{properties := Properties}}) when is_atom(P) ->
    maps:get(P, Properties);
resolve_constraint([{element, _}], {array, #{items := ItemConstraint}}) ->
    ItemConstraint;
resolve_constraint([P | Rest], {object, #{properties := Properties}}) when is_atom(P) ->
    resolve_constraint(Rest, maps:get(P, Properties));
resolve_constraint([{element, _} | Rest], {array, #{items := ItemConstraint}}) ->
    resolve_constraint(Rest, ItemConstraint).

resolve_path([{element, N}], Arr) ->
    lists:nth(N, Arr);
resolve_path([P], Obj) when is_atom(P); is_binary(P) ->
    maps:get(P, Obj);
resolve_path([{element, N} | Rest], Arr) ->
    resolve_path(Rest, lists:nth(N, Arr));
resolve_path([P | Rest], Obj) when is_atom(P); is_binary(P) ->
    resolve_path(Rest, maps:get(P, Obj)).

%% function used by collect for values coming from constrained_integer/0
describe_number({#{min := Min, max := Max}, I, true}) when I < Max, I > Min ->
    {in_range, [min, max]};
describe_number({#{min := Min, max := Max}, I, true}) when I == Max; I == Min ->
    {at_limit, [min, max]};
describe_number({#{min := _, max := _}, _, false}) ->
    {out_of_range, [min, max]};
describe_number({#{min := Min}, I, true}) when I > Min ->
    {in_range, [min]};
describe_number({#{max := Max}, I, true}) when I < Max ->
    {in_range, [max]};
describe_number({#{min := Min}, I, true}) when I =:= Min ->
    {at_limit, [min]};
describe_number({#{max := Max}, I, true}) when I =:= Max ->
    {at_limit, [max]};
describe_number({#{min := _}, _, false}) ->
    {out_of_range, [min]};
describe_number({#{max := _}, _, false}) ->
    {out_of_range, [max]};
describe_number({Constraint, _, true}) when map_size(Constraint) =:= 0 ->
    {in_range, []};
describe_number({_, _, badtype}) ->
    badtype.

cmap_constraint() ->
    cmap_constraint(inf).
cmap_constraint(Depth) ->
    oneof(
        [
            {integer, integer_constraint()},
            {float, float_constraint()},
            {number, number_constraint()},
            {enum, enum_constraint()},
            {string, string_constraint()},
            datetime,
            boolean
        ] ++
            if
                Depth > 0 ->
                    [
                        {array,
                            ?LAZY(
                                array_constraint(
                                    if
                                        Depth == inf -> inf;
                                        true -> max(Depth - 1, 0)
                                    end
                                )
                            )},
                        {object,
                            ?LAZY(
                                object_constraint(
                                    if
                                        Depth == inf -> inf;
                                        true -> max(Depth - 1, 0)
                                    end
                                )
                            )}
                    ];
                true ->
                    []
            end
    ).

integer_constraint() ->
    min_max(integer()).
float_constraint() ->
    min_max(float()).
number_constraint() ->
    min_max(number()).
enum_constraint() ->
    non_empty(list(atom())).
string_constraint() ->
    frequency([{5, #{}}, {95, ?LET(MaxLength, non_neg_integer(), #{max_length => MaxLength})}]).
array_constraint(Depth) ->
    ?LET(
        {LengthConstraints, ItemConstraint},
        {length_constraints(), cmap_constraint(Depth)},
        LengthConstraints#{items => ItemConstraint}
    ).
object_constraint(Depth) ->
    ?LET(
        {{RequiredKeys, OptionalKeys}, AllowExtra},
        {
            object_keys(),
            oneof([#{}, #{additional_properties => false}, #{additional_properties => true}])
        },
        ?LET(
            Specs,
            vector(length(RequiredKeys) + length(OptionalKeys), cmap_constraint(Depth)),
            AllowExtra#{
                properties =>
                    #{K => Spec || {K, Spec} <- lists:zip(RequiredKeys ++ OptionalKeys, Specs)},
                required => RequiredKeys
            }
        )
    ).

object_keys() ->
    ?LET(
        Keys,
        ?LET(Keys, ?LET(Len, integer(0, 8), vector(Len, atom())), lists:uniq(Keys)),
        ?LET(
            N,
            integer(0, length(Keys)),
            begin
                {Required, Optional} = lists:split(N, Keys),
                {Required, Optional}
            end
        )
    ).

length_constraints() ->
    oneof([
        ?LET({X, Y}, {non_neg_integer(), non_neg_integer()}, #{
            min_items => min(X, Y), max_items => max(X, Y)
        }),
        ?LET(X, non_neg_integer(), #{min_items => X}),
        ?LET(X, non_neg_integer(), #{max_items => X}),
        #{}
    ]).

min_max(ValType) ->
    ?LET(
        {X, Y},
        {ValType, ValType},
        oneof([
            #{},
            oneof([#{min => X}, #{min => Y}]),
            oneof([#{max => X}, #{max => Y}]),
            #{max => max(X, Y), min => min(X, Y)}
        ])
    ).

constrained_integer() ->
    ?LET(
        Constraint,
        integer_constraint(),
        ?LET(
            {I, Valid},
            oneof([
                {valid_value({integer, Constraint}), true},
                invalid_value({integer, Constraint})
            ]),
            {Constraint, I, Valid}
        )
    ).

constrained_float() ->
    ?LET(
        Constraint,
        float_constraint(),
        ?LET(
            {F, Valid},
            oneof([
                {valid_value({float, Constraint}), true},
                invalid_value({float, Constraint})
            ]),
            {Constraint, F, Valid}
        )
    ).

constrained_number() ->
    ?LET(
        Constraint,
        number_constraint(),
        ?LET(
            {N, Valid},
            oneof([
                {valid_value({number, Constraint}), true},
                invalid_value({number, Constraint})
            ]),
            {Constraint, N, Valid}
        )
    ).

constrained_string() ->
    ?LET(
        Constraint,
        string_constraint(),
        ?LET(
            {S, Valid},
            oneof([{valid_value({string, Constraint}), true}, invalid_value({string, Constraint})]),
            {Constraint, S, Valid}
        )
    ).

constrained_enum() ->
    ?LET(
        Values,
        enum_constraint(),
        if
            length(Values) > 0 ->
                ?LET(
                    {X, Valid},
                    oneof([{valid_value({enum, Values}), true}, invalid_value({enum, Values})]),
                    {Values, X, Valid}
                );
            length(Values) =:= 0 ->
                ?LET(
                    {X, Valid},
                    invalid_value({enum, Values}),
                    {Values, X, Valid}
                )
        end
    ).

constrained_boolean() ->
    ?LET(
        {X, Valid},
        oneof([{valid_value(boolean), true}, invalid_value(boolean)]),
        {unconstrained, X, Valid}
    ).

constrained_datetime() ->
    ?LET(
        {X, Valid},
        oneof([{valid_value(datetime), true}, invalid_value(datetime)]),
        {unconstained, X, Valid}
    ).

constrained_array() ->
    ?LET(
        Constraint,
        ?LET(D, range(1, 2), array_constraint(D)),
        ?LET(
            {Array, Valid},
            frequency([
                {1, {valid_value({array, Constraint}), true}},
                {6, invalid_value({array, Constraint})}
            ]),
            {Constraint, Array, Valid}
        )
    ).

constrained_object() ->
    ?LET(
        Constraint,
        ?LET(D, range(1, 2), object_constraint(D)),
        ?LET(
            {Obj, Valid},
            frequency([
                {1, {valid_value({object, Constraint}), true}},
                {6, invalid_value({object, Constraint})}
            ]),
            {Constraint, Obj, Valid}
        )
    ).

valid_array(#{items := ItemConstraint, min_items := Min, max_items := Max}) ->
    ?LET(NumExtra, integer(0, Max - Min), vector(Min + NumExtra, valid_value(ItemConstraint)));
valid_array(#{items := ItemConstraint, min_items := Min}) ->
    ?LET(NumExtra, integer(0, inf), vector(Min + NumExtra, valid_value(ItemConstraint)));
valid_array(#{items := ItemConstraint, max_items := Max}) ->
    ?LET(Len, integer(0, Max), vector(Len, valid_value(ItemConstraint)));
valid_array(#{items := ItemConstraint}) ->
    list(valid_value(ItemConstraint)).

subset(Xs) ->
    ?LET(P, float(0.0, 1.0), [X || X <- Xs, rand:uniform() < P]).

valid_object(#{properties := Properties} = Constraints) ->
    Required = maps:get(required, Constraints, []),
    ?LET(
        {RequiredProperties, OptionalProperties},
        {
            [{K, valid_value(maps:get(K, Properties))} || K <- Required],
            subset([{K, valid_value(V)} || K := V <- maps:without(Required, Properties)])
        },
        case maps:get(additional_properties, Constraints, false) of
            true ->
                ?LET(
                    NExtra,
                    integer(0, 8),
                    ?LET(
                        Extra,
                        vector(NExtra, {
                            ?LET(
                                K,
                                ?SUCHTHAT(
                                    K,
                                    utf8(128),
                                    try
                                        AtomK = binary_to_existing_atom(K, utf8),
                                        not is_map_key(AtomK, Properties)
                                    catch
                                        _:_ -> true
                                    end
                                ),
                                frequency([{9, K}, {1, binary_to_atom(K, utf8)}])
                            ),
                            list(
                                oneof([
                                    utf8(),
                                    number(),
                                    atom(),
                                    boolean(),
                                    list([utf8(), number(), atom(), boolean()])
                                ])
                            )
                        }),
                        maps:merge(
                            maps:from_list(Extra),
                            maps:from_list(RequiredProperties ++ OptionalProperties)
                        )
                    )
                );
            false ->
                maps:from_list(RequiredProperties ++ OptionalProperties)
        end
    ).

valid_value({object, Constraints}) ->
    ?LAZY(valid_object(Constraints));
valid_value({array, Constraints}) ->
    ?LAZY(valid_array(Constraints));
valid_value(datetime) ->
    ?LET(
        {UNIXSeconds, Offset},
        {
            resize(100, non_neg_integer()),
            frequency([{3, "Z"}, {1, integer(-3600 * 12, 3600 * 12)}])
        },
        begin
            RFC3339 = calendar:system_time_to_rfc3339(
                UNIXSeconds,
                [{offset, Offset}]
            ),
            Expected = datetime:from_rfc3339(unicode:characters_to_binary(RFC3339)),
            DateTime = calendar:system_time_to_local_time(
                calendar:rfc3339_to_system_time(RFC3339), second
            ),
            frequency([
                {1, unicode:characters_to_binary(RFC3339)},
                {2, DateTime},
                {1, Expected}
            ])
        end
    );
valid_value(boolean) ->
    boolean();
valid_value({enum, Values}) ->
    oneof([oneof(Values), ?LET(V, oneof(Values), atom_to_binary(V, utf8))]);
valid_value({string, Constraint}) ->
    utf8(maps:get(max_length, Constraint, inf));
valid_value({number, Constraint}) ->
    Min = maps:get(min, Constraint, inf),
    Max = maps:get(max, Constraint, inf),
    IGen =
        if
            Max - Min >= 1 ->
                [integer(to_integer(Min, up), to_integer(Max, down))];
            true ->
                []
        end,
    oneof([float(to_float(Min), to_float(Max)) | IGen]);
valid_value({float, Constraint}) ->
    float(maps:get(min, Constraint, inf), maps:get(max, Constraint, inf));
valid_value({integer, Constraint}) ->
    integer(maps:get(min, Constraint, inf), maps:get(max, Constraint, inf)).

to_float(inf) ->
    inf;
to_float(X) ->
    %% this seems dumb
    1.0 * X.

to_integer(inf, _) ->
    inf;
to_integer(X, up) ->
    ceil(X);
to_integer(X, down) ->
    floor(X).

not_unicode(Str) when not is_binary(Str) ->
    true;
not_unicode(Str) ->
    case unicode:characters_to_list(Str) of
        {_, _, _} ->
            true;
        _ ->
            false
    end.

invalid_rfc3339() ->
    ?SUCHTHAT(
        TimeStamp,
        ?LET(
            {{{Y, M, D}, {H, Min, S}}, Offset},
            {
                invalid_localtime(),
                oneof([
                    ?LET(I, integer(), "+" ++ integer_to_list(I)),
                    "Z",
                    ?LET(I, integer(), "asfe" ++ integer_to_list(I)),
                    ""
                ])
            },
            unicode:characters_to_binary(
                io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s", [
                    Y, M, D, H, Min, S, Offset
                ])
            )
        ),
        try
            datetime:from_rfc3339(TimeStamp),
            false
        catch
            _:_ -> true
        end
    ).

invalid_localtime() ->
    oneof([
        {invalid_date(), invalid_time()},
        {invalid_date(), valid_time()},
        {valid_date(), invalid_time()}
    ]).

valid_date() ->
    ?SUCHTHAT(Date, {integer(1, 12), integer(1, 12), integer(1, 31)}, calendar:valid_date(Date)).

invalid_date() ->
    ?SUCHTHAT(
        Date,
        {
            oneof([integer(), neg_integer()]),
            oneof([neg_integer(), integer(13, inf), integer(1, 12)]),
            oneof([neg_integer(), integer(31, inf), integer(1, 30)])
        },
        not calendar:valid_date(Date)
    ).

valid_time() ->
    {integer(0, 23), integer(0, 59), integer(0, 59)}.

invalid_time() ->
    ?SUCHTHAT(
        {H, M, S},
        {
            oneof([neg_integer(), integer(25, inf), integer(0, 24)]),
            oneof([neg_integer(), integer(60, inf), integer(0, 59)]),
            oneof([neg_integer(), integer(60, inf), integer(0, 60)])
        },
        (H < 0 orelse H > 23) orelse (M < 0 orelse M > 59) orelse (S < 0 orelse S > 59)
    ).

bad_length_array(#{min_items := MinLength, max_items := MaxLength, items := ItemConstraint}) when
    MinLength > 0
->
    oneof([too_short_array(MinLength, ItemConstraint), too_long_array(MaxLength, ItemConstraint)]);
bad_length_array(#{min_items := MinLength, max_items := MaxLength, items := ItemConstraint}) when
    MinLength =:= 0
->
    too_long_array(MaxLength, ItemConstraint);
bad_length_array(#{min_items := MinLength, items := ItemConstraint}) ->
    too_short_array(MinLength, ItemConstraint);
bad_length_array(#{max_items := MaxLength, items := ItemConstraint}) ->
    too_long_array(MaxLength, ItemConstraint).

too_short_array(MinLen, ItemConstraint) ->
    ?LET(X, integer(0, max(0, MinLen - 1)), vector(X, valid_value(ItemConstraint))).
too_long_array(MaxLen, ItemConstraint) ->
    ?LET(X, integer(MaxLen + 1, inf), vector(X, valid_value(ItemConstraint))).

bad_element_array(#{max_items := Max}) when Max == 0 ->
    error({'invalid constraint', bad_element_array});
bad_element_array(#{items := ItemConstraint} = Constraint) ->
    ?LET(
        Array,
        non_empty(?LAZY(valid_value({array, Constraint}))),
        with_bad_element(Array, ItemConstraint)
    ).

with_bad_element([_ | Array], ItemConstraint) ->
    ?LET(
        {InvalidItem, Reason},
        ?SUCHTHAT({_, Reason}, invalid_value(ItemConstraint), Reason =/= true),
        ?LET(
            N,
            integer(0, length(Array)),
            begin
                {Before, After} = lists:split(N, Array),
                Array1 = Before ++ [InvalidItem | After],
                {Array1, extend_reason_path(Reason, N + 1)}
            end
        )
    ).

extend_reason_path(Reason = {SubReason, _Properties}, N) when
    is_integer(N) andalso (SubReason =:= missing_properties orelse SubReason =:= illegal_properties)
->
    {Reason, [{element, N}]};
extend_reason_path(Reason = {SubReason, _Properties}, N) when
    is_atom(N) andalso (SubReason =:= missing_properties orelse SubReason =:= illegal_properties)
->
    {Reason, [N]};
extend_reason_path({Reason, Loc}, N) when is_list(Loc), is_integer(N) ->
    {Reason, [{element, N} | Loc]};
extend_reason_path({Reason, Loc}, N) when is_list(Loc), (is_atom(N) or is_binary(N)) ->
    {Reason, [N | Loc]};
extend_reason_path(Reason, N) when is_list(Reason) ->
    [extend_reason_path(R, N) || R <- Reason];
extend_reason_path(Reason, N) when is_integer(N) ->
    {Reason, [{element, N}]};
extend_reason_path(Reason, N) when is_atom(N); is_binary(N) ->
    {Reason, [N]}.

bad_length_and_bad_element_array(#{max_items := Max}) when Max == 0 ->
    error({'invalid constraint', bad_element_array});
bad_length_and_bad_element_array(#{items := ItemConstraint} = Constraint) ->
    ?LET(
        Array,
        non_empty(bad_length_array(Constraint)),
        ?LET(
            {Array1, Reason},
            with_bad_element(Array, ItemConstraint),
            {Array1,
                if
                    is_list(Reason) -> [badlength | Reason];
                    true -> [badlength, Reason]
                end}
        )
    ).

with_extra_property(#{properties := Properties} = Constraints) ->
    ?LET(
        Key,
        ?SUCHTHAT(
            K,
            oneof([atom(), utf8()]),
            if
                is_atom(K) ->
                    not is_map_key(K, Properties);
                is_binary(K) ->
                    try
                        AtomK = binary_to_existing_atom(K, utf8),
                        not is_map_key(AtomK, Properties)
                    catch
                        _:_ ->
                            true
                    end
            end
        ),
        ?LET(
            Obj,
            valid_value({object, Constraints}),
            ?LET(
                T,
                term(),
                {Obj#{Key => T}, {illegal_properties, [Key]}}
            )
        )
    ).

without_required_property(#{required := Required} = Constraints) ->
    ?LET(
        Obj,
        valid_value({object, Constraints}),
        ?LET(
            Without,
            non_empty(subset(Required)),
            {maps:without(Without, Obj), {missing_properties, Without}}
        )
    ).

with_bad_value(#{properties := Properties} = Constraints) ->
    ?LET(
        {Obj, BadProperty},
        {valid_value({object, Constraints}), oneof(maps:keys(Properties))},
        ?LET(
            {BadValue, Reason},
            invalid_value(maps:get(BadProperty, Properties)),
            if
                Reason ->
                    {Obj#{BadProperty => BadValue}, true};
                true ->
                    {Obj#{BadProperty => BadValue}, extend_reason_path(Reason, BadProperty)}
            end
        )
    ).

invalid_value({object, Constraints}) ->
    frequency(
        [
            {1, {?SUCHTHAT(T, term(), not is_map(T)), badtype}}
            | case
                {
                    maps:get(properties, Constraints, #{}),
                    maps:get(required, Constraints, []),
                    maps:get(additional_properties, Constraints, false)
                }
            of
                {Properties, Required, false} when
                    map_size(Properties) > 0,
                    length(Required) > 0
                ->
                    %% All properties are required.
                    [
                        {3, without_required_property(Constraints)},
                        {3, with_bad_value(Constraints)},
                        {1, with_extra_property(Constraints)}
                    ];
                {_, _, false} ->
                    [{1, with_extra_property(Constraints)}];
                _ ->
                    []
            end
        ]
    );
invalid_value({array, Constraint}) ->
    frequency(
        [
            {1, {?SUCHTHAT(T, term(), not is_list(T)), badtype}}
            | case {maps:get(min_items, Constraint, 0), maps:get(max_items, Constraint, inf)} of
                {_, 0} ->
                    [{2, {bad_length_array(Constraint), badlength}}];
                {Min, Max} when Min =:= Max; Min =:= 0, Max =:= inf ->
                    [{5, bad_element_array(Constraint)}];
                {Min, Max} when Min =< 1, Max =:= inf ->
                    %% Can't make an array that is both too short (0 items) and has an invalid item
                    [
                        {2, {bad_length_array(Constraint), badlength}},
                        {5, bad_element_array(Constraint)}
                    ];
                _ ->
                    [
                        {2, {bad_length_array(Constraint), badlength}},
                        {5, bad_element_array(Constraint)},
                        {2, bad_length_and_bad_element_array(Constraint)}
                    ]
            end
        ]
    );
invalid_value(datetime) ->
    oneof([
        ?LET(
            T,
            ?SUCHTHAT(
                T,
                term(),
                case T of
                    {{X, Y, Z}, {P, Q, R}} ->
                        not lists:all(fun is_integer/1, [X, Y, Z, P, Q, R]);
                    _ ->
                        true
                end
            ),
            {T, badtype}
        ),
        {invalid_rfc3339(), badtype},
        {utf8(), badtype},
        {invalid_localtime(), false}
    ]);
invalid_value(boolean) ->
    ?LET(X, ?SUCHTHAT(T, term(), not is_boolean(T)), {X, badtype});
invalid_value({enum, Values}) ->
    oneof([
        ?LET(T, ?SUCHTHAT(T, term(), not is_binary(T) andalso not is_atom(T)), {T, badtype}),
        ?LET(X, ?SUCHTHAT(X, atom(), not lists:member(X, Values)), {X, false}),
        ?LET(
            X,
            ?SUCHTHAT(
                X,
                utf8(),
                try binary_to_existing_atom(X, utf8) of
                    A -> not lists:member(A, Values)
                catch
                    _:_ -> true
                end
            ),
            {X, false}
        )
    ]);
invalid_value({string, Constraint}) ->
    oneof([
        ?LET(T, ?SUCHTHAT(T, term(), not_unicode(T)), {T, badtype}),
        ?LET(T, list(integer(0, 16#10ffff)), {T, badtype})
        | case Constraint of
            #{max_length := MaxLen} ->
                [
                    ?LET(
                        N,
                        integer(1, inf),
                        ?LET(
                            TooLong,
                            ?SUCHTHAT(
                                TooLong,
                                vector(MaxLen + N, integer(0, 16#10ffff)),
                                string:length(TooLong) > MaxLen
                            ),
                            {unicode:characters_to_binary(TooLong), false}
                        )
                    )
                ];
            _ ->
                []
        end
    ]);
invalid_value({number, Constraint}) ->
    oneof([
        ?LET(T, ?SUCHTHAT(T, term(), not is_number(T)), {T, badtype})
        | case Constraint of
            #{min := Min, max := Max} ->
                [
                    ?LET(I, oneof([integer(inf, Min), float(inf, Min)]), {I, I >= Min}),
                    ?LET(I, oneof([integer(Max, inf), float(Max, inf)]), {I, I =< Max})
                ];
            #{min := Min} ->
                [?LET(I, oneof([integer(inf, Min), float(inf, Min)]), {I, I >= Min})];
            #{max := Max} ->
                [?LET(I, oneof([integer(Max, inf), float(Max, inf)]), {I, I =< Max})];
            _ ->
                []
        end
    ]);
invalid_value({float, Constraint}) ->
    oneof([
        ?LET(T, ?SUCHTHAT(T, term(), not is_float(T)), {T, badtype})
        | case Constraint of
            #{min := Min, max := Max} ->
                [
                    ?LET(I, float(inf, Min), {I, I >= Min}),
                    ?LET(I, float(Max, inf), {I, I =< Max})
                ];
            #{min := Min} ->
                [?LET(I, float(inf, Min), {I, I >= Min})];
            #{max := Max} ->
                [?LET(I, float(Max, inf), {I, I =< Max})];
            _ ->
                []
        end
    ]);
invalid_value({integer, Constraint}) ->
    oneof([
        ?LET(T, ?SUCHTHAT(T, term(), not is_integer(T)), {T, badtype})
        | case Constraint of
            #{min := Min, max := Max} ->
                [
                    ?LET(I, integer(inf, Min), {I, I >= Min}),
                    ?LET(I, integer(Max, inf), {I, I =< Max})
                ];
            #{min := Min} ->
                [?LET(I, integer(inf, Min), {I, I >= Min})];
            #{max := Max} ->
                [?LET(I, integer(Max, inf), {I, I =< Max})];
            _ ->
                []
        end
    ]).

load_value(X, undefined) ->
    X;
load_value(X, datetime) when is_binary(X) ->
    datetime:from_rfc3339(X);
load_value({{_, _, _}, {_, _, _}} = X, datetime) ->
    datetime:from_local_time(X);
load_value(X, datetime) ->
    X;
load_value(X, {number, _}) when is_number(X) ->
    X;
load_value(X, {integer, _}) when is_integer(X) ->
    X;
load_value(X, {float, _}) when is_float(X) ->
    X;
load_value(X, {enum, _}) when is_binary(X) ->
    binary_to_existing_atom(X, utf8);
load_value(X, {enum, _}) when is_atom(X) ->
    X;
load_value(X, {string, _}) when is_binary(X) ->
    X;
load_value(X, boolean) when is_boolean(X) ->
    X;
load_value(Xs, {array, #{items := Item}}) when is_list(Xs) ->
    [load_value(X, Item) || X <- Xs];
load_value(X, {object, #{properties := Properties}}) when is_map(X) ->
    #{
        if
            is_map_key(K, Properties) -> to_atom(K);
            true -> K
        end => load_value(V, maps:get(K, Properties, undefined))
     || K := V <- X
    };
load_value(X, undefined) ->
    %% passthrough for completely unconstrained values in an object.
    X;
load_value(X, Spec) ->
    %% TODO this needs to be tweaked so it doean't rely on cmap2
    {ok, #{key := X1}} = cmap:new(#{key => X}, #{properties => #{key => Spec}}),
    X1.

to_atom(X) when is_atom(X) ->
    X;
to_atom(X) when is_binary(X) ->
    binary_to_existing_atom(X, utf8).

min_and_max_number(Gens) ->
    ?LET(
        {Lim1, Lim2},
        {oneof([proper_types:Gen() || Gen <- Gens]), oneof([proper_types:Gen() || Gen <- Gens])},
        begin
            Min = min(Lim1, Lim2),
            Max = max(Lim1, Lim2),
            ?LET(
                {I, Valid},
                oneof(
                    lists:flatten([
                        [
                            %% XXX Find a prettier way to do this.
                            %% Proper's integer/2 and float/2
                            %% generators require the limits to match
                            %% the output type... which makes sense,
                            %% but this is just ugly.
                            if
                                Gen =:= integer ->
                                    {
                                        proper_types:Gen(
                                            trunc(math:ceil(Min)),
                                            trunc(math:floor(Max))
                                        ),
                                        true
                                    };
                                Gen =:= float ->
                                    {proper_types:Gen(float(Min), float(Max)), true}
                            end,
                            if
                                Gen =:= integer ->
                                    ?LET(
                                        I,
                                        proper_types:Gen(inf, trunc(math:ceil(Min))),
                                        {I, I =:= Min}
                                    );
                                Gen =:= float ->
                                    ?LET(I, proper_types:Gen(inf, float(Min)), {I, I =:= Min})
                            end,
                            if
                                Gen =:= integer ->
                                    ?LET(
                                        I,
                                        proper_types:Gen(trunc(math:floor(Max)), inf),
                                        {I, I =:= Max}
                                    );
                                Gen =:= float ->
                                    ?LET(I, proper_types:Gen(float(Max), inf), {I, I =:= Max})
                            end
                        ]
                     || Gen <- Gens
                    ])
                ),
                {#{min => min(Lim1, Lim2), max => max(Lim1, Lim2)}, I, Valid}
            )
        end
    ).

min_only_number(Gens) ->
    ?LET(
        Min,
        oneof([proper_types:Gen() || Gen <- Gens]),
        ?LET(
            {I, Valid},
            oneof(
                lists:flatten([
                    [
                        {proper_types:Gen(Min, inf), true},
                        ?LET(I, proper_types:Gen(inf, Min), {I, I =:= Min})
                    ]
                 || Gen <- Gens
                ])
            ),
            {#{min => Min}, I, Valid}
        )
    ).

max_only_number(Gens) ->
    ?LET(
        Max,
        oneof([proper_types:Gen() || Gen <- Gens]),
        ?LET(
            {I, Valid},
            oneof(
                lists:flatten([
                    [
                        {proper_types:Gen(inf, Max), true},
                        ?LET(I, proper_types:Gen(Max, inf), {I, I =:= Max})
                    ]
                 || Gen <- Gens
                ])
            ),
            {#{max => Max}, I, Valid}
        )
    ).

unconstrained(Gen) ->
    {#{}, Gen, true}.
