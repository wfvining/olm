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
    OL = maps:from_list(Opts),
    [
        {
            Desc ++ " (no keys in spec)",
            ?_assertEqual({ok, #{}}, cmap:new(#{}, OL#{properties => #{}}))
        },
        {Desc, ?_assertEqual({ok, #{}}, cmap:new(#{}, OL#{properties => #{x => {integer, #{}}}}))}
    ].

no_required_with_keys(Opts) ->
    Desc = lists:flatten(
        io_lib:format("construct a map with no required keys (~p) fron a non-empty map", [Opts])
    ),
    OL = maps:from_list(Opts),
    [
        {
            Desc ++ " (no keys in spec)",
            ?_assertEqual(
                {ok, #{x => 1}}, cmap:new(#{x => 1}, OL#{properties => #{x => {integer, #{}}}})
            )
        },
        {Desc,
            ?_assertEqual(
                {ok, #{x => 1}},
                cmap:new(
                    #{x => 1},
                    OL#{properties => #{x => {integer, #{}}, y => {integer, #{}}}}
                )
            )}
    ].

some_required_keys() ->
    Spec = #{properties => #{x => {integer, #{}}, y => {integer, #{}}}, required => [x]},
    [
        {"only required keys are provided",
            ?_assertEqual({ok, #{x => 1}}, cmap:new(#{x => 1}, Spec))},
        {"require and optional keys are provided",
            ?_assertEqual({ok, #{x => 1, y => 2}}, cmap:new(#{x => 1, y => 2}, Spec))},
        {"construction fails when only optional keys are provided",
            ?_assertEqual(
                {error, {property_error, {missing_properties, [x]}}}, cmap:new(#{y => 2}, Spec)
            )},
        {"construction fails when no keys are provided",
            ?_assertEqual(
                {error, {property_error, {missing_properties, [x]}}}, cmap:new(#{}, Spec)
            )}
    ].

all_required_keys() ->
    Spec = #{properties => #{x => {integer, #{}}, y => {integer, #{}}}, required => [x, y]},
    [
        {"construction succeeds when all keys are provided",
            ?_assertEqual(
                {ok, #{x => 1, y => 2}}, cmap:new(#{x => 1, y => 2}, Spec)
            )},
        {"construction fails when no keys are provided",
            ?_assertEqual(
                {error, {property_error, {missing_properties, [x, y]}}},
                cmap:new(#{}, Spec)
            )},
        {"construction fails when only one key is provided", [
            ?_assertEqual(
                {error, {property_error, {missing_properties, [x]}}},
                cmap:new(#{y => 1}, Spec)
            ),
            ?_assertEqual(
                {error, {property_error, {missing_properties, [y]}}},
                cmap:new(#{x => 1}, Spec)
            )
        ]}
    ].

invalid_required_keys() ->
    [
        {"required keys not in spec - no keys in spec/required key specified in input",
            ?_assertEqual(
                {error, {property_error, {illegal_properties, [x]}}},
                cmap:new(#{x => 1}, #{required => [x], properties => #{}})
            )},
        {"required keys not in spec - no keys in spec/no keys in input",
            ?_assertEqual(
                {error, {property_error, {missing_properties, [x]}}},
                cmap:new(#{}, #{required => [x], properties => #{}})
            )},
        {"required keys not in spec/no keys in input",
            ?_assertEqual(
                {error, {property_error, {missing_properties, [x]}}},
                cmap:new(#{}, #{properties => #{y => {integer, #{}}}, required => [x]})
            )},
        {"required keys not in spec/valid key in input",
            ?_assertEqual(
                {error, {property_error, {missing_properties, [x]}}},
                cmap:new(#{y => 1}, #{properties => #{y => {integer, #{}}}, required => [x]})
            )},
        {"required keys not in spec/invalid (required) key in input",
            ?_assertEqual(
                {error, {property_error, {illegal_properties, [x]}}},
                cmap:new(#{x => 2}, #{properties => #{y => {integer, #{}}}, required => [x]})
            )},
        {"required keys not in spec/invalid (required) key and valid key in input",
            ?_assertEqual(
                {error, {property_error, {illegal_properties, [x]}}},
                cmap:new(#{x => 1, y => 2}, #{properties => #{y => {integer, #{}}}, required => [x]})
            )}
    ].

extra_keys_test_() ->
    [
        {"extra keys not required",
            ?_assertEqual(
                {ok, #{}}, cmap:new(#{}, #{properties => #{}, additional_properties => true})
            )}
    ].
