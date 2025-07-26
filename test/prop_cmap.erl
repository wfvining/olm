-module(prop_cmap).

-include_lib("proper/include/proper.hrl").

-compile([export_all]).

cmap_integer_spec() ->
    ?LET(Constraint, integer_constraint(), {integer, Constraint}).

cmap_boolean_spec() ->
    {boolean, #{}}.

cmap_number_spec() ->
    {number, #{}}.

cmap_datetime_spec() ->
    {datetime, #{}}.

cmap_string_spec() ->
    ?LET(Constraint, string_constraint(), {string, Constraint}).

cmap_enum_spec() ->
    ?LET(Values, enumeration(), {enum, Values}).

cmap_list_spec() ->
    cmap_list_spec(0).

cmap_list_spec(Depth, MinLen, MaxLen) ->
    ?LET(LengthConstraints, list_length_constraint(MinLen, MaxLen),
         ?LET(ItemSpec, ?LAZY(cmap_value_spec(Depth - 1)), {list, LengthConstraints#{items => ItemSpec}})).

cmap_list_spec(Depth) ->
    ?LET(LengthConstraints, list_length_constraint(),
         ?LET(ItemSpec, ?LAZY(cmap_value_spec(Depth - 1)), {list, LengthConstraints#{items => ItemSpec}})).

cmap_object_spec() ->
    cmap_object_spec(0).

cmap_object_spec(Depth) ->
    ?LET(Keys, 
         ?LET(L, list(atom()), lists:uniq(L)), 
         ?LET({Specs, I}, 
              {[{K, ?LAZY(cmap_value_spec(Depth - 1))} || K <- Keys], integer(0, length(Keys))},
              {object, {maps:from_list(Specs), [{required, lists:nthtail(I, Keys)}]}})).

cmap_value_spec(Depth) when Depth =< 0 ->
    oneof([cmap_integer_spec(),
           cmap_boolean_spec(),
           cmap_number_spec(),
           cmap_datetime_spec(),
           cmap_string_spec()]);
cmap_value_spec(Depth) ->
    oneof([cmap_integer_spec(),
           cmap_boolean_spec(),
           cmap_number_spec(),
           cmap_datetime_spec(),
           cmap_string_spec(),
           cmap_list_spec(Depth),
           cmap_object_spec(Depth)]).

enumeration() ->
    ?SUCHTHAT(Vals, ?LET(L, list(atom()), lists:uniq(L)), length(Vals) > 0).

list_length_constraint(Min, Max) ->
    ?LET({X, Y},
         {integer(Min, Max), integer(Min, Max)},
         ?LET(LC, oneof(
                    if 
                        Max < inf, Min > 0 ->
                            [[{min_length, min(X, Y)}, {max_length, max(X, Y)}]];
                        Min > 0 ->
                            [[{min_length, min(X, Y)}],
                             [{min_length, min(X, Y)}, {max_length, max(X, Y)}]];
                        Max < inf ->
                            [[{max_length, max(X, Y)}],
                             [{min_length, min(X, Y)}, {max_length, max(X, Y)}]];
                        Min =:= 0, Max =:= inf ->
                            [[], [{min_length, min(X, Y)}], [{max_length, max(X, Y)}],
                             [{min_length, min(X, Y)}, {max_length, max(X, Y)}]]
                    end),
              maps:from_list(LC))).

list_length_constraint() ->
    list_length_constraint(0, inf).

integer_constraint() ->
    ?LET({X, Y},
         {integer(), integer()},
         begin
             Min = min(X, Y),
             Max = max(X, Y),
             oneof([#{}, #{min => Min}, #{max => Max}, #{min => Min, max => Max}])
         end).

satisfiable_integer_constraint() ->
    ?LET({X, Y},
         {integer(), integer()},
         begin
             Min = min(X, Y),
             Max = max(X, Y),
             oneof([#{}, #{min => Min}, #{max => Max}, #{min => Min, max => Max}])
         end).

unsatisfiable_integer_constraint() ->
    ?LET({X, Y}, 
         ?SUCHTHAT({X, Y}, {integer(), integer()}, Y =/= X),
         #{min => max(X, Y), max => min(X, Y)}).

string_constraint() ->
    frequency([{4, ?LET(Len, non_neg_integer(), #{max_length => Len})},
               {1, #{}}]).

string_constraint(I) ->
    ?LET(Len, integer(I, inf), #{max_length => Len}).

cmap_value({integer, Constraint}) ->
    ?LET(I, integer(maps:get(min, Constraint, inf), maps:get(max, Constraint, inf)), {I, I});
cmap_value({string, #{max_length := MaxLen}}) ->
    ?LET(Str, utf8_string(MaxLen), cmap_string_value(Str));
cmap_value({string, _}) ->
    ?LET(Str, utf8_string(), cmap_string_value(Str));
cmap_value({boolean, _}) ->
    ?LET(B, boolean(), {B, B});
cmap_value({number, _}) ->
    ?LET(N, number(), {N, N});
cmap_value({datetime, _}) ->
    cmap_datetime_value();
cmap_value({list, #{items := ItemSpec} = Constraints}) ->
    MinLen = maps:get(min_length, Constraints, 0),
    MaxLen = maps:get(max_length, Constraints, inf),
    cmap_list_value(MinLen, MaxLen, cmap_value(ItemSpec));
cmap_value({object, {Properties, Options}}) ->
    ?LET(Vals, [{K, cmap_value(ValSpec)} || K := ValSpec <- Properties],
         begin
             Optional = maps:keys(Properties) -- proplists:get_value(required, Options, []),
             ?LET(Missing, someof(Optional), 
                  begin
                      InputObj = maps:from_list([{K, Input} || {K, {Input, _}} <- Vals]),
                      OutputObj = maps:from_list([{K, Output} || {K, {_, Output}} <- Vals]),
                      {maps:without(Missing, InputObj), maps:without(Missing, OutputObj)}
                  end)
         end);
cmap_value({enum, Values}) ->
    ?LET(Val, oneof(Values), ?LET(V, oneof([atom_to_binary(Val), Val]), {V, Val})).

cmap_constructor({integer, Constraints}) when map_size(Constraints) =:= 0 ->
    oneof([fun cmap:integer/1, cmap:integer_(Constraints)]);
cmap_constructor({integer, Constraints}) ->
    cmap:integer_(Constraints);
cmap_constructor({string, Constraints}) when map_size(Constraints) =:= 0 ->
    oneof([fun cmap:string/1, cmap:string_(Constraints)]);
cmap_constructor({string, Constraints}) ->
    cmap:string_(Constraints);
cmap_constructor({boolean, _}) ->
    fun cmap:boolean/1;
cmap_constructor({number, _}) ->
    fun cmap:number/1;
cmap_constructor({datetime, _}) ->
    fun cmap:datetime/1;
cmap_constructor({list, #{items := ItemSpec} = Constraints}) ->
    ?LET(Fun, cmap_constructor(ItemSpec), cmap:list_(Constraints#{items => Fun}));
cmap_constructor({list, Constraints}) ->
    cmap:list_(Constraints);
cmap_constructor({object, {Properties, Options}}) ->
    ?LET(Constructors,
         [{K, cmap_constructor(Spec)} || K := Spec <- Properties],
         fun(X) -> cmap:new(maps:from_list(Constructors), X, Options) end);
cmap_constructor({enum, Values}) ->
    cmap:enum_(Values).

someof(Vals) ->
    ?LET(I, integer(0, length(Vals)), choose_from(I, queue:from_list(Vals))).

choose_from(0, _) ->
    [];
choose_from(N, Vals) ->
    H = queue:head(Vals),
    T = queue:tail(Vals),
    ?LET(Take, boolean(), if Take -> [H|choose_from(N - 1, T)]; true -> choose_from(N, queue:in(H, T)) end).

cmap_list_value(MinLen, MaxLen, ElementGen) ->
    ?LET(List, list(ElementGen),
         if length(List) < MinLen ->
                 ?LET(List1, cmap_list_value(MinLen - length(List), 
                                             if MaxLen == inf -> inf; true -> MaxLen - length(List) end,
                                             ElementGen), 
                      lists:unzip(List ++ lists:zip(element(1, List1), element(2, List1))));
            length(List) >= MinLen, length(List) =< MaxLen ->
                 lists:unzip(List);
            length(List) > MaxLen ->
                 lists:unzip(lists:nthtail(length(List) - MaxLen, List))
         end).

local_datetime() ->
    ?SUCHTHAT(
       {Date, _Time},
       {{integer(1970, 9999), integer(1, 12), integer(1, 31)}, {
           integer(0, 23), integer(0, 59), integer(0, 59)
       }},
       calendar:valid_date(Date)
      ).

%% This won't be needed in OTP 28
to_system_time(DateTime) when DateTime >= {{1970, 1, 1}, {0, 0, 0}} ->
    Offset = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    Secs - Offset.

cmap_datetime_value() ->
    ?LET(DateTime, local_datetime(),
         begin
             RFC3339 = calendar:system_time_to_rfc3339(to_system_time(DateTime)),
             frequency([
               {1, {RFC3339, DateTime}},
               {1, {unicode:characters_to_binary(RFC3339), DateTime}},
               {2, {DateTime, DateTime}}])
         end).

cmap_string_value(Str) ->
    BinStr = unicode:characters_to_binary(Str),
    oneof([{Str, BinStr}, {BinStr, BinStr}]).

%%% Generators for {Ctor, Input, Output} or {Ctor, Constraints, Input, Output} tuples

spec_value(SpecGen) ->
    ?LET(Spec = {_, Constraint}, SpecGen,
         ?LET({Ctor, {InVal, OutVal}}, {cmap_constructor(Spec), cmap_value(Spec)},
              {Ctor, Constraint, InVal, OutVal})).

cmap_integer() ->
    spec_value(cmap_integer_spec()).

cmap_number() ->
    spec_value(cmap_number_spec()).

cmap_string() ->
    spec_value(cmap_string_spec()).

cmap_datetime() ->
    spec_value(cmap_datetime_spec()).

cmap_enum() ->
    spec_value(cmap_enum_spec()).

cmap_object() ->
    cmap_object(0).
cmap_object(Depth) ->
    spec_value(cmap_object_spec(Depth)).

cmap_list() ->
    cmap_list(0).
cmap_list(Depth) ->
    spec_value(cmap_list_spec(Depth)).

%%% Other useful generators

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

cmap_key() ->
    ?LET(A, atom(), {A, oneof([A, atom_to_binary(A)])}).

%%% Properties

-define(maybe_error(Expr, ExpectedGood, ExpectedErrors, ErrorExpected),
        maybe_error(fun () -> Expr end, ExpectedGood, ExpectedErrors, ErrorExpected)).

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
        {{Fun, _, DT, DateTime}, {Key, K}},
        {cmap_datetime(), cmap_key()},
        begin
            #{Key => DateTime} =:= cmap:new(#{Key => Fun}, #{K => DT})
        end
    ).

prop_cmap_not_datetime() ->
    ?FORALL(
        {{Fun, _, _, _}, NotDateTime, {Key, K}},
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

prop_cmap_list_too_short() ->
    ?FORALL({Len, Ctor, Items},
            ?LET(ListSpec = {_, #{min_length := Len}},
                 cmap_list_spec(0, 1, inf),
                 ?LET({Ctor, {InVals, _}}, 
                      {cmap_constructor(ListSpec), cmap_value(ListSpec)},
                      {Len, Ctor, InVals})),
            begin
                {ShortList, _} = lists:split(Len - 1, Items),
                ?maybe_error(
                   cmap:new(#{foo => Ctor}, #{foo => ShortList}),
                   nil,
                   [{badvalue, {too_short, ShortList}}],
                   true
                  )
            end).
            
prop_cmap_list_too_long() ->
    ?FORALL({Ctor, Items},
            ?LET(ListSpec = {_, #{max_length := Len} = Constraints},
                 ?SUCHTHAT({_, Constraint}, cmap_list_spec(),
                           maps:get(max_length, Constraint, inf) < inf),
                 ?LET({Ctor, {InVals, _}}, 
                      {cmap_constructor(ListSpec), 
                       cmap_value({list, maps:without([max_length], Constraints#{min_length => Len + 1})})},
                      {Ctor, InVals})),
            begin
                ?maybe_error(
                   cmap:new(#{foo => Ctor}, #{foo => Items}),
                   nil,
                   [{badvalue, {too_long, Items}}],
                   true
                  )
            end).

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

prop_cmap_list() ->
    ?FORALL({{Fun, _, Input, Output}, {Key, K}}, 
            {cmap_list(), cmap_key()},
            #{Key => Output} =:= cmap:new(#{Key => Fun}, #{K => Input})).

prop_cmap_deep_list() ->
    ?FORALL({{Fun, _, Inputs, Outputs}, {Key, K}},
            {?LET(Depth, integer(1, 5), cmap_list(Depth)), cmap_key()},
            #{Key => Outputs} =:= cmap:new(#{Key => Fun}, #{K => Inputs})).

anything_but({IType, _} = Spec) ->
    Options = [{integer, cmap_integer_spec()},
               {boolean, cmap_boolean_spec()},
               {number, cmap_number_spec()},
               {datetime, cmap_datetime_spec()},
               {string, cmap_string_spec()},
               {list, cmap_list_spec(0)},
               {object, cmap_object_spec(0)}],
    Gens = [Gen || {T, Gen} <- Options, T =/= IType],
    ?LET(Ctor,
         cmap_constructor(Spec),
         ?SUCHTHAT({Val, _},
                   ?LET(BSpec, 
                        oneof(Gens),
                        ?LET(Vals, cmap_value(BSpec), Vals)),
                   try Ctor(Val) of _ -> false catch _:_ -> true end)).

invalid_list() ->
    ?LET({_, Constraints} = ListSpec,
         ?SUCHTHAT(
            {list, Constr},
            ?LET(I, pos_integer(), cmap_list_spec(2, I, inf)), 
            is_map_key(items, Constr)),
         begin
         ?LET({BadItem, Ctor},
              ?LET({Item, ItemOut},
                   anything_but(maps:get(items, Constraints)),
                   {?LET(I, oneof([Item, ItemOut]), I), cmap_constructor(ListSpec)}),
              ?LET({Vals, _}, cmap_value(ListSpec),
                   ?LET({P, Ps},
                        ?SUCHTHAT(
                           {P, Ps}, 
                           {?SUCHTHAT(P, float(0.0, 1.0), P > 0.0), vector(length(Vals), float(0.0, 1.0))},
                           lists:any(fun(X) -> X < P end, Ps)),
                        {Ctor, Constraints,
                         [if X < P -> BadItem; X >= P -> Val end 
                          || {X, Val} <- lists:zip(Ps, Vals)]}))) end).

prop_cmap_list_invalid() ->
    ?FORALL(
        {Fun, _Constraint, Inputs},
        invalid_list(),
        try cmap:new(#{foo => Fun}, #{foo => Inputs}) of
            E -> 
                io:format("Incorrect -> ~p~n", [E]),
                false
        catch
            error:{badvalue, {baditem, _}} ->
                true
        end
    ).

prop_wrong_type() ->
    ?FORALL(
       {Ctor, {BadIn, BadOut}},
       ?LET(GoodSpec,
            resize(2, ?SIZED(Size, cmap_value_spec(Size))),
            {cmap_constructor(GoodSpec), anything_but(GoodSpec)}),
       try Ctor(BadIn) of 
           _ -> false 
       catch _:_ ->
               try Ctor(BadOut) of
                   _ -> false
               catch _:_ ->
                       true
               end
       end).

prop_encode_json() ->
    ?FORALL(
       {Ctor, {ObjIn, Obj}},
       ?LET(Spec, cmap_value_spec(4), 
            {cmap_constructor(Spec), cmap_value(Spec)}),
       begin
           O = Ctor(ObjIn),
           Encoded = cmap:to_json(O),
           Decoded = json:decode(iolist_to_binary(Encoded)),
           Obj =:= Ctor(Decoded)
       end).
