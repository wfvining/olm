-module(test_cmap2).

-include_lib("eunit/include/eunit.hrl").

string_test_() ->
    [
        {
            ~S"'\r\n' is a single grapheme in unicode",
            ?_test(
                begin
                    Obj = #{foo => <<$\r, $\n>>},
                    Spec = #{properties => #{foo => {string, #{max_length => 1}}}},
                    ?assertMatch({ok, _}, cmap2:new(Obj, Spec))
                end
            )
        }
    ].

illegal_property_test_() ->
    [
        {"a sub object with an illegal property keyed by a binary causes an error",
            ?_test(
                begin
                    Obj = #{foo => #{~B"bar" => 1}},
                    Spec = #{
                        properties => #{foo => {object, #{properties => #{}, required => []}}}
                    },
                    ?assertEqual(
                        {error,
                            {{property_error, {illegal_properties, [~B"bar"]}}, #{
                                key => [foo],
                                value => #{~B"bar" => 1},
                                spec => {object, #{properties => #{}, required => []}}
                            }}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )},
        {"a sub object with an illegal property keyed by an atom causes an error",
            ?_test(
                begin
                    Obj = #{foo => #{bar => 1}},
                    Spec = #{
                        properties => #{foo => {object, #{properties => #{}, required => []}}}
                    },
                    ?assertEqual(
                        {error,
                            {{property_error, {illegal_properties, [bar]}}, #{
                                key => [foo],
                                value => #{bar => 1},
                                spec => {object, #{properties => #{}, required => []}}
                            }}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )},
        {"a sub-sub object with an illegal property keyed by an atom causes an error",
            ?_test(
                begin
                    Obj = #{foo => #{bar => #{baz => 1}}},
                    Spec = #{
                        properties => #{
                            foo =>
                                {object, #{properties => #{bar => {object, #{properties => #{}}}}}}
                        }
                    },
                    ?assertEqual(
                        {error,
                            {{property_error, {illegal_properties, [baz]}}, #{
                                key => [foo, bar],
                                value => #{baz => 1},
                                spec => {object, #{properties => #{}}}
                            }}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )}
    ].

missing_property_test_() ->
    [
        {"a sub object with a missing required property causes an error",
            ?_test(
                begin
                    Obj = #{foo => #{}},
                    Spec = #{
                        properties => #{
                            foo =>
                                {object, #{
                                    properties => #{bar => {number, #{}}}, required => [bar]
                                }},
                            required => [foo]
                        }
                    },
                    ?assertEqual(
                        {error,
                            {{property_error, {missing_properties, [bar]}}, #{
                                key => [foo],
                                value => #{},
                                spec =>
                                    {object, #{
                                        properties => #{bar => {number, #{}}}, required => [bar]
                                    }}
                            }}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )},
        {"a sub-sub object with a missing required property causes an error",
            ?_test(
                begin
                    Obj = #{key => #{foo => #{}}},
                    Spec = #{
                        properties => #{
                            key =>
                                {object, #{
                                    properties => #{
                                        foo =>
                                            {object, #{
                                                properties => #{bar => {number, #{}}},
                                                required => [bar]
                                            }}
                                    },
                                    required => [foo]
                                }}
                        }
                    },

                    ?assertEqual(
                        {error,
                            {{property_error, {missing_properties, [bar]}}, #{
                                key => [key, foo],
                                value => #{},
                                spec =>
                                    {object, #{
                                        properties => #{bar => {number, #{}}}, required => [bar]
                                    }}
                            }}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )}
    ].

type_error_test_() ->
    [
        {"a sub object that has a type error causes a type error when loading its parent",
            ?_test(
                begin
                    Obj = #{key => #{foo => true}},
                    Spec = #{
                        properties => #{key => {object, #{properties => #{foo => {integer, #{}}}}}}
                    },
                    ?assertEqual(
                        {error,
                            {type_error, #{
                                key => [key, foo], value => true, spec => {integer, #{}}
                            }}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )},
        {"a top level object with a property that violates a type constriant resutls in a type error",
            ?_test(
                begin
                    Obj = #{foo => true},
                    Spec = #{properties => #{foo => {integer, #{}}}},
                    ?assertEqual(
                        {error,
                            {type_error, #{key => [foo], value => true, spec => {integer, #{}}}}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )},
        {"a type error in an array results in a type error for the object",
            ?_test(
                begin
                    Obj = #{foo => [~B"a", ~B"c", 16#0D]},
                    Spec = #{properties => #{foo => {array, #{items => {string, #{}}}}}},
                    ?assertEqual(
                        {error,
                            {type_error, #{
                                key => [foo, {element, 3}], value => 16#0D, spec => {string, #{}}
                            }}},
                        cmap2:new(Obj, Spec)
                    )
                end
            )}
    ].
