-module(prop_cmap).

-include_lib("proper/include/proper.hrl").

-compile([export_all]).

cmap_integer() ->
    frequency([{20, constrained_integer()}, {5, unconstrained_integer()}]).

cmap_string() ->
    frequency([{20, constrained_string()}, {5, unconstrained_string()}]).

cmap_datetime() ->
    ?LET(
        {DT, ParsedDT},
        timestamp_or_datetime(),
        {fun cmap:datetime/1, DT, ParsedDT}
    ).

cmap_enum() ->
    ?LET(
        Values,
        ?SUCHTHAT(Values, list(atom()), length(Values) > 0),
        ?LET(
            {Value, EnumValue},
            enum_value(Values),
            {cmap:enum_(Values), Values, Value, EnumValue}
        )
    ).

cmap_list_primitive_items() ->
    ?LET(
        Val,
        oneof([
            ?LAZY({string, cmap_string()}),
            ?LAZY({enum, cmap_enum()}),
            ?LAZY(
                {integer,
                    ?SUCHTHAT(
                        {_, Constraint, _, _},
                        cmap_integer(),
                        maps:get(min, Constraint, maps:get(max, Constraint, inf)) =<
                            maps:get(max, Constraint, inf)
                    )}
            ),
            ?LAZY({datetime, cmap_datetime()})
        ]),
        case Val of
            {string, {Fun, Constraint, _, _}} ->
                ?LET(
                    List,
                    list(0, inf, constrained_string(Constraint)),
                    begin
                        Vals = [{Value, Parsed} || {_, _, Value, Parsed} <- List],
                        {InputValues, ParsedValues} = lists:unzip(Vals),
                        {
                            cmap:list_(#{items => Fun}),
                            #{items => Constraint},
                            InputValues,
                            ParsedValues
                        }
                    end
                );
            {enum, {Fun, EnumValues, _, _}} ->
                ?LET(
                    List,
                    list(0, inf, enum_value(EnumValues)),
                    begin
                        {InputValues, ParsedValues} = lists:unzip(List),
                        {
                            cmap:list_(#{items => Fun}),
                            #{items => EnumValues},
                            InputValues,
                            ParsedValues
                        }
                    end
                );
            {integer, {Fun, Constraint, _, _}} ->
                ?LET(
                    List,
                    list(0, inf, constrained_integer(Constraint)),
                    begin
                        Vals = [V || {_, _, _, V} <- List],
                        {cmap:list_(#{items => Fun}), #{items => Constraint}, Vals, Vals}
                    end
                );
            {datetime, {Fun, _, _}} ->
                ?LET(
                    List,
                    list(0, inf, timestamp_or_datetime()),
                    begin
                        {InputValues, ParsedValues} = lists:unzip(List),
                        {
                            cmap:list_(#{items => Fun}),
                            #{items => datetime},
                            InputValues,
                            ParsedValues
                        }
                    end
                )
        end
    ).

cmap_primitive() ->
    ?LET(
        Val,
        oneof([
            ?LAZY({string, cmap_string()}),
            ?LAZY({enum, cmap_enum()}),
            ?LAZY(
                {integer,
                    ?SUCHTHAT(
                        {_, Constraint, _, _},
                        cmap_integer(),
                        maps:get(min, Constraint, maps:get(max, Constraint, inf)) =<
                            maps:get(max, Constraint, inf)
                    )}
            ),
            ?LAZY({datetime, cmap_datetime()})
        ]),
        case Val of
            {string, {Fun, Constraint, _, _}} ->
                {string, Fun, Constraint};
            {enum, {Fun, EnumValues, _, _}} ->
                {enum, Fun, EnumValues};
            {integer, {Fun, Constraint, _, _}} ->
                {integer, Fun, Constraint};
            {datetime, {Fun, _, _}} ->
                {datetime, Fun, nil}
        end
    ).

cmap_list_objects() ->
    ?LET(
        {Constructor, Spec, _Keys, _Required, _Extra} = ObjSpec,
        cmap_object_spec(),
        %% TODO Required and extra
        ?LET(
            Objs,
            list(cmap_object(ObjSpec)),
            begin
                {Input, Output} = lists:unzip(Objs),
                %% TODO Not sure this spec is what I want, but it matches cmap_list_primitive_itmes()
                {cmap:list_(#{items => Constructor}), #{items => Spec}, Input, Output}
            end
        )
    ).

cmap_object_spec() ->
    ?LET(
        Keys,
        ?LET(L, list(atom()), lists:uniq(L)),
        ?LET(
            {KeySpecs, I},
            {[{K, cmap_primitive()} || K <- Keys], integer(1, max(1, length(Keys)))},
            begin
                Spec = maps:from_list([{K, Ctor} || {K, {_, Ctor, _}} <- KeySpecs]),
                Required =
                    if
                        Keys =:= [] -> [];
                        true -> element(1, lists:split(I, Keys))
                    end,
                {
                    fun(X) -> cmap:new(Spec, X, [{required, Required}]) end,
                    {Spec, KeySpecs},
                    Keys,
                    Required,
                    false
                }
            end
        )
    ).

cmap_object({_Ctor, {_Spec, Constraints}, Keys, Required, _Extra}) ->
    %% TODO extra keys
    ?LET(
        Vals,
        [{K, cmap_primitive_value(ValueSpec)} || {K, ValueSpec} <- Constraints],
        begin
            Optional = Keys -- Required,
            %%io:format("optional keys: ~p~nrequired keys: ~p~n", [Optional, Required]),
            ?LET(
                Missing,
                subset(Optional),
                begin
                    %%io:format("Missing = ~p~n", [Missing]),
                    InputObj = maps:from_list([{K, Input} || {K, {Input, _}} <- Vals]),
                    OutputObj = maps:from_list([{K, Output} || {K, {_, Output}} <- Vals]),
                    {maps:without(Missing, InputObj), maps:without(Missing, OutputObj)}
                end
            )
        end
    ).

subset(Elements) ->
    ?LET(
        L,
        proper_types:fixed_list([oneof([[], E]) || E <- Elements]),
        lists:flatten(L)
    ).

cmap_primitive_value({string, _, Constraint}) ->
    ?LET({_, _, Input, Output}, constrained_string(Constraint), {Input, Output});
cmap_primitive_value({enum, _, EnumValues}) ->
    enum_value(EnumValues);
cmap_primitive_value({integer, _, Constraint}) ->
    ?LET({_, _, _, Value}, constrained_integer(Constraint), {Value, Value});
cmap_primitive_value({datetime, _, _}) ->
    timestamp_or_datetime().

enum_value(Values) ->
    ?LET(
        EnumValue,
        oneof(Values),
        {oneof([atom_to_binary(EnumValue), atom_to_list(EnumValue), EnumValue]), EnumValue}
    ).

string_constraint() ->
    frequency(
        [
            {15, ?LET(Len, non_neg_integer(), #{max_length => Len})},
            {1, #{}}
        ]
    ).

string_constraint(I) ->
    ?LET(Len, integer(I, inf), #{max_length => Len}).

integer_constraint() ->
    frequency(
        [
            {2,
                ?LET(
                    {Min, Max},
                    ?SUCHTHAT({Min, Max}, {integer(), integer()}, Max < Min),
                    #{min => Min, max => Max}
                )},
            {2, #{}},
            {5, ?LET(MinMax, integer(), #{min => MinMax, max => MinMax})},
            {12, ?LET(Max, integer(), #{max => Max})},
            {12, ?LET(Min, integer(), #{min => Min})},
            {12,
                ?LET(
                    {Min, Max},
                    ?SUCHTHAT({Min, Max}, {integer(), integer()}, Max > Min),
                    #{min => Min, max => Max}
                )}
        ]
    ).

unconstrained_string() ->
    ?LET(
        Str,
        unicode_string(),
        oneof([
            {fun cmap:string/1, #{}, Str, unicode:characters_to_binary(Str)},
            {
                fun cmap:string/1,
                #{},
                unicode:characters_to_binary(Str),
                unicode:characters_to_binary(Str)
            }
        ])
    ).

constrained_string() ->
    constrained_string(string_constraint()).

constrained_string(Constraints) when is_map(Constraints) ->
    Constructor = cmap:string_(Constraints),
    oneof([
        ?LET(
            Str,
            unicode_string(Constraints),
            {Constructor, Constraints, Str, unicode:characters_to_binary(Str)}
        ),
        ?LET(
            Str,
            unicode_string(Constraints),
            {Constructor, Constraints, unicode:characters_to_binary(Str),
                unicode:characters_to_binary(Str)}
        )
    ]);
constrained_string(Constraint) ->
    ?LET(Constraints, Constraint, constrained_string(Constraints)).

unicode_string() ->
    unicode_string(#{}).
unicode_string(Constraints) when map_size(Constraints) =:= 0 ->
    utf8_string();
unicode_string(#{max_length := MaxLen}) ->
    utf8_string(MaxLen).

long_string(MinLen) ->
    ?SUCHTHAT(
        Str,
        oneof([utf8_string((MinLen + 1) * 2), utf8((MinLen + 1) * 2)]),
        string:length(Str) > MinLen
    ).

not_string_list() ->
    ?SUCHTHAT(
        List,
        list(
            oneof([?SUCHTHAT(I, integer(), (I < 0) or (I > 16#10ffff)), atom(), float(), tuple()])
        ),
        length(List) > 0
    ).

not_string() ->
    oneof([integer(), atom(), float(), tuple(), not_string_list()]).

nonempyt_string() ->
    ?SUCHTHAT(Str, unicode_string(), string:length(Str) > 0).

timestamp_or_datetime() ->
    oneof([?LET(DT, local_datetime(), {DT, DT}), rfc3339()]).

local_datetime() ->
    ?SUCHTHAT(
        {Date, _Time},
        {{integer(1970, 9999), integer(1, 12), integer(1, 31)}, {
            integer(0, 23), integer(0, 59), integer(0, 59)
        }},
        calendar:valid_date(Date)
    ).

%% This won't be needed in OTP 28
to_system_time(DateTime) when DateTime > {{1970, 1, 1}, {0, 0, 0}} ->
    Offset = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    Secs - Offset.

rfc3339() ->
    ?LET(
        DateTime,
        local_datetime(),
        {
            oneof([
                unicode:characters_to_binary(
                    calendar:system_time_to_rfc3339(to_system_time(DateTime))
                ),
                calendar:system_time_to_rfc3339(to_system_time(DateTime))
            ]),
            DateTime
        }
    ).

not_datetime() ->
    ?SUCHTHAT(
        DT,
        oneof([term(), malformed_datetime()]),
        case DT of
            {{_, _, _}, {H, M, S}} when
                is_integer(H), is_integer(M), is_integer(S)
            ->
                (H > 23) orelse (H < 0) orelse
                    (M > 59) orelse (M < 0) orelse
                    (M > 59) orelse (M < 0);
            {Date, {H, M, S}} when
                is_integer(H), is_integer(M), is_integer(S)
            ->
                not calendar:valid_date(Date);
            DT when is_list(DT); is_binary(DT) ->
                try calendar:rfc3339_to_system_time(DT) of
                    _ -> false
                catch
                    _:_ ->
                        true
                end;
            _ ->
                true
        end
    ).

malformed_datetime() ->
    ?SUCHTHAT(
        {Date, {H, M, S}},
        {{integer(), integer(), integer()}, {integer(), integer(), integer()}},
        (not calendar:valid_date(Date)) orelse
            (H < 0) orelse (H > 23) orelse
            (M < 0) orelse (M > 59) orelse
            (S < 0) orelse (S > 59)
    ).

unconstrained_integer() ->
    ?LET(I, integer(), {fun cmap:integer/1, #{}, I, I}).

constrained_integer() ->
    constrained_integer(integer_constraint()).

constrained_integer(Constraints) when is_map(Constraints) ->
    Min = maps:get(min, Constraints, inf),
    Max = maps:get(max, Constraints, inf),
    if
        Min > Max, (Min =/= inf) and (Max =/= inf) ->
            {cmap:integer_(Constraints), Constraints, impossible, impossible};
        true ->
            ?LET(V, integer(Min, Max), {cmap:integer_(Constraints), Constraints, V, V})
    end;
constrained_integer(Constraint) ->
    ?LET(Constraints, Constraint, constrained_integer(Constraints)).

cmap_key() ->
    oneof([?LET(A, atom(), {A, A}), ?LET(A, atom(), {A, atom_to_binary(A)})]).

list(MinLen, MaxLen) ->
    list(MinLen, MaxLen, term()).

list(MinLen, MaxLen, ElemGen) ->
    ?LET(List, list(ElemGen), list(MinLen, MaxLen, List, ElemGen)).

list(_MinLen, MaxLen, List, _) when MaxLen =/= inf, length(List) >= MaxLen ->
    {L, _} = lists:split(MaxLen, List),
    L;
list(MinLen, MaxLen, List, ElemGen) when length(List) < MinLen ->
    ?LET(L, list(ElemGen), list(MinLen, MaxLen, List ++ L, ElemGen));
list(MinLen, MaxLen, List, ElemGen) when
    length(List) >= MinLen,
    length(List) < MaxLen
->
    oneof([List, ?LET(L, list(ElemGen), list(MinLen, MaxLen, List ++ L, ElemGen))]).

cmap_length_constrained_list() ->
    ?LET(
        {Min, Max},
        ?SUCHTHAT({Min, Max}, {non_neg_integer(), non_neg_integer()}, Min =< Max),
        ?LET(
            Constraint,
            oneof(
                [
                    #{},
                    #{max_length => Max},
                    #{min_length => Min},
                    #{min_length => Min, max_length => Max}
                ]
            ),
            {
                cmap:list_(Constraint),
                Constraint,
                list(maps:get(min_length, Constraint, 0), maps:get(max_length, Constraint, inf))
            }
        )
    ).

-define(maybe_error(Expr, ExpectedGood, ExpectedErrors, ErrorExpected),
    try
        (ExpectedGood =:= (Expr)) andalso (not (ErrorExpected))
    catch
        error:badarg ->
            (Expr);
        error:Err ->
            (ErrorExpected) andalso lists:any(fun(E) -> Err =:= (E) end, ExpectedErrors)
    end
).

maybe_error(Fun, ExpectedGood, ExpectedErrors, ErrorExpected) ->
    try
        (ExpectedGood =:= Fun()) andalso (not (ErrorExpected))
    catch
        error:badarg ->
            Fun();
        error:Err ->
            ErrorExpected andalso lists:any(fun(E) -> Err =:= (E) end, ExpectedErrors)
    end.

prop_cmap_integer() ->
    ?FORALL(
        {{Fun, Constraints, GoodI, _}, I, {Key, K}},
        {cmap_integer(), integer(), cmap_key()},
        begin
            Min = maps:get(min, Constraints, undefined),
            Max = maps:get(max, Constraints, undefined),
            Spec = #{Key => Fun},
            ?maybe_error(
                cmap:new(Spec, #{K => GoodI}),
                #{Key => GoodI},
                [{badvalue, {invalid, GoodI}}],
                not is_integer(GoodI)
            ) andalso
                if
                    ((I =< Max) andalso (Min =:= undefined) andalso (Max =/= undefined));
                    ((I >= Min) andalso (Max =:= undefined) andalso (Min =/= undefined));
                    (((I =< Max) andalso (I >= Min)) andalso (Min =/= undefined) andalso
                        (Max =/= undefined));
                    ((Min =:= undefined) andalso (Max =:= undefined)) ->
                        #{Key => I} =:= cmap:new(Spec, #{K => I});
                    true ->
                        try
                            cmap:new(Spec, #{K => I}),
                            false
                        catch
                            error:{badvalue, {invalid, I}} ->
                                true
                        end
                end
        end
    ).

prop_cmap_string() ->
    ?FORALL(
        {{Fun, _Constraints, Str, ParsedStr}, NotStr, {Key, K}},
        {cmap_string(), not_string(), cmap_key()},
        begin
            Spec = #{Key => Fun},
            #{Key => ParsedStr} =:= cmap:new(Spec, #{K => Str}) andalso
                ?maybe_error(
                    cmap:new(Spec, #{K => NotStr}),
                    nil,
                    [{badvalue, {not_string, NotStr}}, {badvalue, {not_unicode, NotStr}}],
                    true
                )
        end
    ).

prop_cmap_string_too_long() ->
    ?FORALL(
        {{Fun, Constraint, BadStr}, {Key, K}},
        {
            ?LET(
                #{max_length := MaxLen} = Constraint,
                string_constraint(0),
                {cmap:string_(Constraint), Constraint, long_string(MaxLen)}
            ),
            cmap_key()
        },
        begin
            MaxLen = maps:get(max_length, Constraint),
            Len = string:length(BadStr),
            ?maybe_error(
                cmap:new(#{Key => Fun}, #{K => BadStr}),
                nil,
                [{badvalue, {string_too_long, [{len, Len}, {limit, MaxLen}]}}],
                true
            )
        end
    ).

prop_cmap_datetime() ->
    ?FORALL(
        {{Fun, DT, DateTime}, {Key, K}},
        {cmap_datetime(), cmap_key()},
        begin
            #{Key => DateTime} =:= cmap:new(#{Key => Fun}, #{K => DT})
        end
    ).

prop_cmap_not_datetime() ->
    ?FORALL(
        {{Fun, _, _}, NotDateTime, {Key, K}},
        {cmap_datetime(), not_datetime(), cmap_key()},
        begin
            Errs =
                case NotDateTime of
                    {NotDate, NotTime} ->
                        [
                            {badvalue, {invalid_datetime, NotDateTime}},
                            {badvalue, {invalid_date, NotDate}},
                            {badvalue, {invalid_time, NotTime}}
                        ];
                    NotDateTime when is_binary(NotDateTime) ->
                        [{badvalue, {invalid_datetime, binary_to_list(NotDateTime)}}];
                    _ ->
                        [{badvalue, {invalid_datetime, NotDateTime}}]
                end,
            ?maybe_error(cmap:new(#{Key => Fun}, #{K => NotDateTime}), nil, Errs, true)
        end
    ).

prop_cmap_enum() ->
    ?FORALL(
        {{Fun, _EnumVals, Value, EnumValue}, {Key, K}},
        {cmap_enum(), cmap_key()},
        #{Key => EnumValue} =:= cmap:new(#{Key => Fun}, #{K => Value})
    ).

prop_cmap_invalid_enum() ->
    ?FORALL(
        {Fun, _EnumVals, BadVal, {Key, K}},
        ?LET(
            {Fun, EnumVals, _, _},
            cmap_enum(),
            {Fun, EnumVals,
                ?SUCHTHAT(
                    V,
                    frequency([{20, atom()}, {20, utf8()}, {20, utf8_string()}, {5, term()}]),
                    (not is_binary(V) andalso not is_list(V) andalso not is_atom(V)) orelse
                        cant_be_atom(V) orelse
                        (is_atom(V) andalso not lists:member(V, EnumVals)) orelse
                        (is_binary(V) andalso not lists:member(binary_to_atom(V), EnumVals)) orelse
                        (is_list(V) andalso not lists:member(list_to_atom(V), EnumVals))
                ),
                cmap_key()}
        ),
        ?maybe_error(
            cmap:new(#{Key => Fun}, #{K => BadVal}),
            nil,
            [{badvalue, {invalid_enum_value, BadVal}}],
            true
        )
    ).

cant_be_atom(V) when is_binary(V) ->
    try binary_to_atom(V) of
        _ ->
            false
    catch
        _:_ ->
            true
    end;
cant_be_atom(V) when is_list(V) ->
    try list_to_atom(V) of
        _ ->
            false
    catch
        _:_ ->
            true
    end;
cant_be_atom(_) ->
    false.

prop_cmap_boolean() ->
    ?FORALL(
        {Bool, {Key, K}},
        {boolean(), cmap_key()},
        #{Key => Bool} =:= cmap:new(#{Key => fun cmap:boolean/1}, #{K => Bool})
    ).

prop_cmap_not_bool() ->
    ?FORALL(
        {Term, {Key, K}},
        {?SUCHTHAT(Term, term(), not is_boolean(Term)), cmap_key()},
        ?maybe_error(
            cmap:new(#{Key => fun cmap:boolean/1}, #{K => Term}),
            nil,
            [{badvalue, {not_boolean, Term}}],
            true
        )
    ).

prop_cmap_number() ->
    ?FORALL(
        {Num, {Key, K}},
        {number(), cmap_key()},
        #{Key => Num} =:= cmap:new(#{Key => fun cmap:number/1}, #{K => Num})
    ).

prop_cmap_not_number() ->
    ?FORALL(
        {Term, {Key, K}},
        {?SUCHTHAT(Term, term(), not is_number(Term)), cmap_key()},
        ?maybe_error(
            cmap:new(#{Key => fun cmap:number/1}, #{K => Term}),
            nil,
            [{badvalue, {not_number, Term}}],
            true
        )
    ).

prop_cmap_list_length() ->
    ?FORALL(
        {{Fun, _Constraint, List}, {Key, K}},
        {cmap_length_constrained_list(), cmap_key()},
        #{Key => List} =:= cmap:new(#{Key => Fun}, #{K => List})
    ).

prop_cmap_list_too_short() ->
    ?FORALL(
        {MinLen, {Key, K}},
        {pos_integer(), cmap_key()},
        begin
            List = lists:duplicate(MinLen - 1, foo),
            ?maybe_error(
                cmap:new(
                    #{Key => cmap:list_(#{min_length => MinLen})},
                    #{K => List}
                ),
                nil,
                [{badvalue, {too_short, List}}],
                true
            )
        end
    ).

prop_cmap_list_too_long() ->
    ?FORALL(
        {MaxLen, {Key, K}},
        {non_neg_integer(), cmap_key()},
        begin
            List = lists:duplicate(MaxLen + 1, foo),
            ?maybe_error(
                cmap:new(
                    #{Key => cmap:list_(#{max_length => MaxLen})},
                    #{K => List}
                ),
                nil,
                [{badvalue, {too_long, List}}],
                true
            )
        end
    ).

prop_cmap_list_not_list() ->
    ?FORALL(
        {Term, {Key, K}},
        {?SUCHTHAT(Term, term(), not is_list(Term)), cmap_key()},
        begin
            P1 = maybe_error(
                fun() -> cmap:new(#{Key => fun cmap:list/1}, #{K => Term}) end,
                nil,
                [{badvalue, {not_list, Term}}],
                true
            ),
            P2 = maybe_error(
                fun() -> cmap:new(#{Key => cmap:list_(#{})}, #{K => Term}) end,
                nil,
                [{badvalue, {not_list, Term}}],
                true
            ),
            P1 and P2
        end
    ).

prop_cmap_list_primitive_items() ->
    ?FORALL(
        {{Fun, _Constraint, Inputs, Outputs}, {Key, K}},
        {cmap_list_primitive_items(), cmap_key()},
        #{Key => Outputs} =:= cmap:new(#{Key => Fun}, #{K => Inputs})
    ).

prop_cmap_list_objects() ->
    ?FORALL(
        {{Fun, _Constraint, Inputs, Outputs}, {Key, K}},
        {cmap_list_objects(), cmap_key()},
        #{Key => Outputs} =:= cmap:new(#{Key => Fun}, #{K => Inputs})
    ).

prop_cmap_list_invalid() ->
    ?FORALL(
        {Fun, _Constraint, Inputs},
        ?LET(
            {Fun, Constraint, Inputs, Outputs},
            oneof([cmap_list_objects(), cmap_list_primitive_items()]),
            ?LET(
                Objs,
                ?SUCHTHAT(
                    Objs,
                    list(
                        1,
                        inf,
                        if
                            length(Inputs) > 0 ->
                                frequency(
                                    [
                                        {1, oneof(lists:zip(Inputs, Outputs))},
                                        {4, ?LET(V, cmap_primitive(), cmap_primitive_value(V))}
                                    ]
                                );
                            length(Inputs) =:= 0 ->
                                ?LET(V, cmap_primitive(), cmap_primitive_value(V))
                        end
                    ),
                    begin
                        {Inputs1, _} = lists:unzip(Objs),
                        try Fun(Inputs1) of
                            _ -> false
                        catch
                            _:_ -> true
                        end
                    end
                ),
                begin
                    {Inputs1, _} = lists:unzip(Objs),
                    {Fun, Constraint, Inputs1}
                end
            )
        ),
        try cmap:new(#{foo => Fun}, #{foo => Inputs}) of
            _ -> false
        catch
            error:{badvalue, {baditem, _}} ->
                true
        end
    ).
