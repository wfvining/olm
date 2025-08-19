-module(test_cmap).

-include_lib("eunit/include/eunit.hrl").

required_keys_test_() ->
    [
        no_required_keys(),
        some_required_keys(),
        all_required_keys(),
        invalid_required_keys()
    ].

no_required_keys() ->
    [
        no_required_empty_input([]),
        no_required_empty_input([{reqired, []}]),
        no_required_with_keys([]),
        no_required_with_keys([{required, []}])
    ].

no_required_empty_input(Opts) ->
    Desc = lists:flatten(
        io_lib:format("construct a map with no required keys (~p) from an empty input map", [Opts])
    ),
    [
        {Desc ++ " (no keys in spec)", ?_assertEqual(#{}, cmap:new(#{}, #{}, Opts))},
        {Desc, ?_assertEqual(#{}, cmap:new(#{x => fun cmap:integer/1}, #{}, Opts))}
    ].

no_required_with_keys(Opts) ->
    Desc = lists:flatten(
        io_lib:format("construct a map with no required keys (~p) fron a non-empty map", [Opts])
    ),
    [
        {
            Desc ++ " (no keys in spec)",
            ?_assertEqual(#{x => 1}, cmap:new(#{x => fun cmap:integer/1}, #{x => 1}, Opts))
        },
        {Desc,
            ?_assertEqual(
                #{x => 1},
                cmap:new(
                    #{x => fun cmap:integer/1, y => fun cmap:integer/1},
                    #{x => 1},
                    Opts
                )
            )}
    ].

some_required_keys() ->
    Spec = #{x => fun cmap:integer/1, y => fun cmap:integer/1},
    [
        {"only required keys are provided",
            ?_assertEqual(#{x => 1}, cmap:new(Spec, #{x => 1}, [{required, [x]}]))},
        {"require and optional keys are provided",
            ?_assertEqual(#{x => 1, y => 2}, cmap:new(Spec, #{x => 1, y => 2}, [{required, [x]}]))},
        {"construction fails when only optional keys are provided",
            ?_assertError({missing_keys, [x]}, cmap:new(Spec, #{y => 2}, [{required, [x]}]))},
        {"construction fails when no keys are provided",
            ?_assertError({missing_keys, [x]}, cmap:new(Spec, #{}, [{required, [x]}]))}
    ].

all_required_keys() ->
    Spec = #{x => fun cmap:integer/1, y => fun cmap:integer/1},
    [
        {"construction succeeds when all keys are provided",
            ?_assertEqual(
                #{x => 1, y => 2}, cmap:new(Spec, #{x => 1, y => 2}, [{required, [x, y]}])
            )},
        {"construction fails when no keys are provided",
            ?_assertError({missing_keys, [x, y]}, cmap:new(Spec, #{}, [{required, [x, y]}]))},
        {"construction fails when only one key is provided", [
            ?_assertError({missing_keys, [x]}, cmap:new(Spec, #{y => 1}, [{required, [x, y]}])),
            ?_assertError({missing_keys, [y]}, cmap:new(Spec, #{x => 1}, [{required, [x, y]}]))
        ]}
    ].

invalid_required_keys() ->
    [
        {"required keys not in spec - no keys in spec/required key specified in input",
            ?_assertError(badarg, cmap:new(#{}, #{x => 1}, [{required, [x]}]))},
        {"required keys not in spec - no keys in spec/no keys in input",
            ?_assertError(badarg, cmap:new(#{}, #{}, [{required, [x]}]))},
        {"required keys not in spec/no keys in input",
            ?_assertError(badarg, cmap:new(#{y => fun cmap:integer/1}, #{}, [{required, [x]}]))},
        {"required keys not in spec/valid key in input",
            ?_assertError(
                badarg, cmap:new(#{y => fun cmap:integer/1}, #{y => 1}, [{required, [x]}])
            )},
        {"required keys not in spec/invalid (required) key in input",
            ?_assertError(
                badarg, cmap:new(#{y => fun cmap:integer/1}, #{x => 2}, [{required, [x]}])
            )},
        {"required keys not in spec/invalid (required) key and valid key in input",
            ?_assertError(
                badarg, cmap:new(#{y => fun cmap:integer/1}, #{x => 1, y => 2}, [{required, [x]}])
            )}
    ].

extra_keys_test_() ->
    [
        {"extra keys not allowed by default", [
            ?_assertError({badvalue, {extra_key, x}}, cmap:new(#{}, #{x => 1})),
            ?_assertError(
                {badvalue, {extra_key, x}}, cmap:new(#{y => fun cmap:integer/1}, #{x => 1})
            ),
            ?_assertError({badvalue, {extra_key, <<"x">>}}, cmap:new(#{}, #{<<"x">> => 1}))
        ]},
        {"extra keys allowed/atom",
            ?_assertEqual(#{<<"x">> => 1}, cmap:new(#{}, #{x => 1}, [extra_keys]))},
        {"extra keys allowed/binary",
            ?_assertEqual(#{<<"x">> => 1}, cmap:new(#{}, #{<<"x">> => 1}, [extra_keys]))},
        {"extra keys allowed/string",
            ?_assertEqual(#{<<"x">> => 1}, cmap:new(#{}, #{"x" => 1}, [extra_keys]))},
        {"extra keys not required", ?_assertEqual(#{}, cmap:new(#{}, #{}, [extra_keys]))}
    ].
