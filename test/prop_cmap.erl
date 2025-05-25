-module(prop_cmap).

-include_lib("proper/include/proper.hrl").

-compile(exprt_all).

integer_limits() ->
    oneof([
        [{min, integer()}, {max, integer()}],
        [{min, integer()}],
        [{max, integer()}],
        []
    ]).

integer_constraints() ->
    ?LET(Lims, integer_limits(), maps:from_list(Lims)).

prop_integer_limits() ->
    ?FORALL(
        {Constraints, I, K},
        {integer_constraints(), integer(), oneof([foo, <<"foo">>])},
        begin
            Max = maps:get(max, Constraints, undefined),
            Min = maps:get(min, Constraints, undefined),
            if
                I =< Max, Min =:= undefined, Max =/= undefined ->
                    #{foo => I} =:= cmap:new(#{foo => cmap:integer_(Constraints)}, #{K => I});
                I >= Min, Max =:= undefined, Min =/= undefined ->
                    #{foo => I} =:= cmap:new(#{foo => cmap:integer_(Constraints)}, #{K => I});
                (I =< Max) andalso (I >= Min), Min =/= undefined, Max =/= undefined ->
                    #{foo => I} =:= cmap:new(#{foo => cmap:integer_(Constraints)}, #{K => I});
                Min =:= undefined, Max =:= undefined ->
                    #{foo => I} =:= cmap:new(#{foo => cmap:integer_(Constraints)}, #{K => I});
                true ->
                    try
                        cmap:new(#{foo => cmap:integer_(Constraints)}, #{K => I}),
                        false
                    catch
                        error:{badvalue, {invalid, I}} ->
                            true
                    end
            end
        end
    ).

prop_integer() ->
    ?FORALL(
        T,
        term(),
        begin
            if
                is_integer(T) ->
                    #{foo => T} =:= cmap:new(#{foo => fun cmap:integer/1}, #{foo => T});
                true ->
                    try
                        cmap:new(#{foo => fun cmap:integer/1}, #{foo => T}),
                        false
                    catch
                        error:{badvalue, {not_an_integer, T}} ->
                            true
                    end
            end
        end
    ).

binary_unicode_string() ->
    ?LET(Str, string(), unicode:characters_to_binary(Str)).

prop_binary_string() ->
    ?FORALL(
        Str,
        binary_unicode_string(),
        #{foo => Str} =:= cmap:new(#{foo => fun cmap:string/1}, #{foo => Str})
    ).

prop_list_string() ->
    ?FORALL(
        Str,
        string(),
        begin
            BinStr = unicode:characters_to_binary(Str),
            #{foo => BinStr} =:= cmap:new(#{foo => fun cmap:string/1}, #{foo => Str})
        end
    ).

prop_bad_string() ->
    ?FORALL(
        Str,
        string(),
        begin
            Str1 = [16#10ffff + 1 | Str],
            try
                cmap:new(#{foo => fun cmap:string/1}, #{foo => Str1}),
                false
            catch
                error:{badvalue, {not_unicode, Str1}} ->
                    true
            end
        end
    ).

prop_too_long_string() ->
    ?FORALL(
        {Str, Char},
        {string(), char()},
        begin
            Len = string:length(Str),
            Str1 = [Char | Str],
            try
                cmap:new(#{foo => cmap:string_(#{max_length => Len})}, #{foo => Str1}),
                false
            catch
                error:{badvalue, {string_too_long, _}} ->
                    true
            end
        end
    ).

prop_string_length() ->
    ?FORALL(
        {Str, Len},
        {string(), proper_types:integer(0, 1024)},
        begin
            StrLen = string:length(Str),
            if
                StrLen =< Len ->
                    Str1 = unicode:characters_to_binary(Str),
                    #{foo => Str1} =:=
                        cmap:new(#{foo => cmap:string_(#{max_length => Len})}, #{foo => Str});
                StrLen > Len ->
                    try
                        cmap:new(#{foo => cmap:string_(#{max_length => Len})}, #{foo => Str}),
                        false
                    catch
                        error:{badvalue, {string_too_long, _}} ->
                            true
                    end
            end
        end
    ).

to_system_time(DateTime) when DateTime > {{1970, 1, 1}, {0, 0, 0}} ->
    Offset = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    Secs - Offset.

datetime() ->
    ?SUCHTHAT(
        {{_, _, _} = Date, {_, _, _}},
        {{integer(1970, 3000), integer(1, 12), integer(1, 31)}, {
            integer(0, 23), integer(0, 59), integer(0, 59)
        }},
        calendar:valid_date(Date)
    ).

rfc3339() ->
    ?LET(
        DateTime, datetime(), {DateTime, calendar:system_time_to_rfc3339(to_system_time(DateTime))}
    ).

prop_datetime() ->
    ?FORALL(
        {DateTime, Str},
        rfc3339(),
        begin
            Bin = list_to_binary(Str),
            #{foo => DateTime} =:= cmap:new(#{foo => fun cmap:datetime/1}, #{foo => Str}) andalso
                #{foo => DateTime} =:= cmap:new(#{foo => fun cmap:datetime/1}, #{foo => Bin}) andalso
                #{foo => DateTime} =:= cmap:new(#{foo => fun cmap:datetime/1}, #{foo => DateTime})
        end
    ).

bad_datetime() ->
    %% Maybe bad date, maybe bad time such that bad(date) or bad(time)
    oneof([bad_dt(), bad_rfc3339()]).

bad_dt() ->
    ?SUCHTHAT(
        {Date, _Time},
        {{integer(), integer(), integer()}, {integer(), integer(), integer()}},
        not calendar:valid_date(Date)
    ).

bad_rfc3339() ->
    oneof([
        bad_well_formed_datetime(),
        ?SUCHTHAT(
            DT,
            maybe_ill_formed_datetime(),
            not is_integer(catch calendar:rfc3339_to_system_time(DT))
        )
    ]).

bad_well_formed_datetime() ->
    ?LET(
        {{Y, M, D}, {H, Min, S}},
        bad_dt(),
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Y, M, D, H, Min, S]
        )
    ).

maybe_ill_formed_datetime() ->
    ?LET(
        {{Y, M, D}, {H, Min, S}, DPartSep, TSep, TPartSep},
        {
            {integer(), integer(), integer()},
            {integer(), integer(), integer()},
            char(),
            char(),
            char()
        },
        io_lib:format(
            "~B~c~B~c~B~c~B~c~B~c~BZ",
            [Y, DPartSep, M, DPartSep, D, TSep, H, TPartSep, Min, TPartSep, S]
        )
    ).

prop_bad_datetime() ->
    ?FORALL(
        BadDatetime,
        bad_datetime(),
        try
            %% TODO This only checks for the datetime type input,
            %%      what about invalid RFC3339 strings
            cmap:new(#{foo => fun cmap:datetime/1}, #{foo => BadDatetime}),
            false
        catch
            error:{badvalue, {Err, _}} when
                Err =:= invalid_time;
                Err =:= invalid_date;
                Err =:= invalid_datetime
            ->
                true
        end
    ).

prop_enum() ->
    ?FORALL(
        {EnumVals, InvalidVal},
        ?SUCHTHAT(
            {EnumVals, InvalidVal},
            {list(atom()), atom()},
            not lists:member(InvalidVal, EnumVals)
        ),
        begin
            Spec = #{foo => cmap:enum_(EnumVals)},
            Vals =
                [{atom_to_binary(X), X} || X <- EnumVals] ++
                    [{atom_to_list(X), X} || X <- EnumVals] ++
                    [{X, X} || X <- EnumVals],
            Valid = lists:all(
                fun({X, Val}) ->
                    #{foo => Val} =:= cmap:new(Spec, #{foo => X})
                end,
                Vals
            ),
            Valid andalso
                lists:all(
                    fun(X) ->
                        try cmap:new(Spec, #{foo => X}) of
                            _ -> false
                        catch
                            error:{badvalue, {invalid_enum_value, _}} ->
                                true
                        end
                    end,
                    [InvalidVal, atom_to_binary(InvalidVal), atom_to_list(InvalidVal)]
                )
        end
    ).

prop_key_type() ->
    ?FORALL(
        K,
        atom(),
        begin
            Spec = #{K => fun cmap:integer/1},
            #{K => 1} =:= cmap:new(Spec, #{K => 1}) andalso
                #{K => 1} =:= cmap:new(Spec, #{atom_to_list(K) => 1}) andalso
                #{K => 1} =:= cmap:new(Spec, #{atom_to_binary(K) => 1})
        end
    ).

prop_unknown_keys() ->
    ?FORALL(
        Key,
        ?SUCHTHATMAYBE(
            Key,
            string(),
            try
                list_to_existing_atom(Key),
                false
            catch
                error:badarg -> true
            end
        ),
        begin
            K = unicode:characters_to_binary(Key),
            #{K => 1} =:= cmap:new(#{}, #{Key => 1}, [extra_keys]) andalso
                #{K => 1} =:= cmap:new(#{}, #{unicode:characters_to_binary(Key) => 1}, [extra_keys]) andalso
                #{K => 1} =:= cmap:new(#{}, #{list_to_atom(Key) => 1}, [extra_keys]) andalso
                #{K => 1} =:= cmap:new(#{}, #{Key => 1}, [extra_keys])
        end
    ).

prop_boolean() ->
    ?FORALL(
        Val,
        oneof([proper_types:boolean(), proper_types:term()]),
        begin
            if
                is_boolean(Val) ->
                    #{foo => Val} =:= cmap:new(#{foo => fun cmap:boolean/1}, #{foo => Val});
                true ->
                    try
                        cmap:new(#{foo => fun cmap:boolean/1}, #{foo => Val}),
                        false
                    catch
                        error:{badvalue, {not_boolean, _}} ->
                            true
                    end
            end
        end
    ).

prop_number() ->
    ?FORALL(
        Val,
        oneof([proper_types:number(), proper_types:term()]),
        begin
            if
                is_number(Val) ->
                    #{foo => Val} =:= cmap:new(#{foo => fun cmap:number/1}, #{foo => Val});
                true ->
                    try
                        cmap:new(#{foo => fun cmap:number/1}, #{foo => Val}),
                        false
                    catch
                        error:{badvalue, {not_number, _}} ->
                            true
                    end
            end
        end
    ).
