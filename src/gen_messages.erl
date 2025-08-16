-module(gen_messages).

-export([main/1]).

-include_lib("syntax_tools/include/merl.hrl").

main(Args) ->
    argparse:run(Args, cli(), #{progname => gen_messages}).

cli() ->
    #{
        arguments =>
            [
                #{name => schemadir, help => "Path to directory containing JSON schemas."},
                #{
                    name => module,
                    help => "Name of the output module",
                    type => {atom, unsafe}
                },
                #{
                    name => outdir,
                    help => "Directory where the output file should be written",
                    short => $o,
                    long => "-outdir",
                    type => string
                }
            ],
        handler => fun gen/1
    }.

gen(Args = #{schemadir := Path, module := Module}) ->
    {ok, Files} = file:list_dir(Path),
    Schemas = [filename:join(Path, File) || File <- Files, filename:extension(File) =:= ".json"],
    {Exports, Types, Specs, Constructors} = lists:foldl(
        fun add_schema/2, {[], [], [], []}, Schemas
    ),
    {TypeNames, TypeForms} = lists:unzip(lists:ukeysort(1, Types)),
    TypeFormStrs = [
        lists:concat([erl_prettypr:format(Form, [{paper, 88}, {ribbon, 88}]) || Form <- Forms])
     || Forms <- TypeForms
    ],
    {_SpecNames, SpecForms} = lists:unzip(lists:ukeysort(1, Specs)),
    SpecFormStrs = [erl_prettypr:format(Form, [{paper, 88}, {ribbon, 88}]) || Form <- SpecForms],
    {_CNames, CtorForms} = lists:unzip(lists:ukeysort(1, Constructors)),
    CtorStrs = [
        string:join([erl_prettypr:format(Form, [{paper, 88}, {ribbon, 88}]) || Form <- Forms], "\n")
     || Forms <- CtorForms
    ],
    ModAttr =
        erl_syntax:attribute(
            erl_syntax:atom(module), [erl_syntax:atom(Module)]
        ),
    Export = ?Q("-export(['@_@funcs'/1]).", [{funcs, Exports}]),
    GenMsgType =
        "-type message(Payload) :: " ++
            erl_prettypr:format(
                erl_syntax:tuple([
                    erl_syntax:macro(erl_syntax:variable("MODULE")),
                    erl_syntax:user_type_application(erl_syntax:atom(payload_type), []),
                    erl_syntax:variable("Payload")
                ])
            ) ++ ".",
    MsgType = "-type message() :: message(payload()).",
    ExportNames = [erl_syntax:atom_value(erl_syntax:arity_qualifier_body(Func)) || Func <- Exports],
    PayloadType =
        "-type payload_type() :: \n" ++
            string:join(
                [
                    "    " ++ Line
                 || Line <- string:split(
                        erl_prettypr:format(
                            erl_syntax:type_union([
                                erl_syntax:atom(Type)
                             || Type <- TypeNames,
                                lists:member(
                                    list_to_atom(snake_case(atom_to_list(Type))),
                                    ExportNames
                                )
                            ])
                        ),
                        "\n",
                        all
                    )
                ],
                "\n"
            ) ++ ".",
    Payload =
        "-type payload() :: \n" ++
            string:join(
                [
                    "    " ++ Line
                 || Line <- string:split(
                        erl_prettypr:format(
                            erl_syntax:type_union(
                                [
                                    erl_syntax:type_application(erl_syntax:atom(Type), [])
                                 || Type <- TypeNames,
                                    lists:member(
                                        list_to_atom(snake_case(atom_to_list(Type))), ExportNames
                                    )
                                ]
                            )
                        ),
                        "\n",
                        all
                    )
                ],
                "\n"
            ) ++ ".",
    %% XXX The following two statements are a hack around peculiar behavior from erl_prettypr.
    %%     If you provide an attribute tree with name 'export_type' to erl_prettypr:format/1,
    %%     the formating will choke on the body of the contained arity qualifier trees when it
    %%     attempts to convert the atom to a list.
    %%
    %%     Because this only happens if the attribute name is exactly 'export_type', we can work
    %%     around it by changing the attribute name and then substituting in the string after
    %%     the syntax tree has been formatted.
    ExportTypes = ?Q(
        "-eXpOrT_TyPe(['@_@types'/0]).",
        [
            {types, [
                erl_syntax:arity_qualifier(
                    erl_syntax:atom(Type),
                    erl_syntax:integer(0)
                )
             || Type <- TypeNames
            ]}
        ]
    ),
    ExportTypes1 = string:replace(erl_prettypr:format(ExportTypes), "eXpOrT_TyPe", "export_type"),
    ExportMessageType = "-export_type([message/0, message/1, payload_type/0, payload/0]).",
    OutDir = maps:get(outdir, Args, "."),
    Datetime = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    Admonition =
        "%% DO NOT EIDT THIS FILE\n"
        "%% It was generated by gen_messages.erl from " ++ Path ++
            "\n"
            "%% " ++ Datetime,
    Decode = ?Q(
        [
            "-spec decode(MessageType :: binary(), Direction :: request | response,"
            " Payload :: json:decode_value()) -> {ok, message()} | {error, Reason :: any()}.",
            "decode(MessageType, request, Payload) ->",
            "decode(binary_to_existing_atom(<<MessageType/binary, \"Request\">>), Payload);"
            "decode(MessageType, response, Payload) ->",
            "decode(binary_to_existing_atom(<<MessageType/binary, \"Response\">>), Payload).",
            "-spec decode(PayloadType :: payload_type(), Payload :: json:decode_value()) "
            "-> {ok, message()} | {error, Reason :: any()}.",
            "decode(PayloadType, Payload) ->",
            "try PayloadType(Payload) of",
            "Message ->",
            "{ok, {_@module, PayloadType, Message}}",
            "catch",
            "_:Reason ->",
            "{error, Reason}",
            "end."
        ],
        [{module, erl_syntax:macro(erl_syntax:variable("MODULE"))}]
    ),
    file:write_file(
        filename:join([OutDir, atom_to_list(Module) ++ ".erl"]),
        string:join(
            [
                Admonition,
                erl_prettypr:format(ModAttr),
                erl_prettypr:format(Export),
                "-export([decode/3, decode/2]).",
                ExportTypes1,
                ExportMessageType,
                GenMsgType,
                MsgType,
                PayloadType,
                Payload
                | TypeFormStrs
            ] ++ SpecFormStrs ++ CtorStrs ++
                [
                    string:join(
                        [
                            erl_prettypr:format(Expr, [{paper, 88}, {ribbon, 88}])
                         || Expr <- Decode
                        ],
                        "\n"
                    )
                ],
            "\n\n"
        )
    ).

add_schema(File, {ExistingExports, ExistingTypeForms, ExistingSpecDefs, ExistingConstructors}) ->
    {ok, Schema} = file:read_file(File),
    Obj = json:decode(Schema),
    TypeName = type_id(Obj),
    TypeForms = load_types(Obj, TypeName),
    SpecDefs = load_specs(Obj, TypeName),
    [{CName, _} | _] = Constructors = load_constructors(Obj, TypeName),
    Export = erl_syntax:arity_qualifier(erl_syntax:atom(CName), erl_syntax:integer(1)),
    {
        [Export | ExistingExports],
        TypeForms ++ ExistingTypeForms,
        SpecDefs ++ ExistingSpecDefs,
        Constructors ++ ExistingConstructors
    }.

type_id(Obj) when is_map_key(<<"$id">>, Obj) ->
    lists:last(
        string:split(
            maps:get(<<"$id">>, Obj), ":", all
        )
    );
type_id(Obj) when is_map_key(<<"id">>, Obj) ->
    lists:last(
        string:split(
            maps:get(<<"id">>, Obj), ":", all
        )
    ).

load_constructors(Obj, TypeName) ->
    SnakeName = snake_case(binary_to_list(TypeName)),
    Spec = ?Q(
        "-spec '@name'('@tname'()) -> message('@tname'()).",
        [
            {name, erl_syntax:atom(SnakeName)},
            {tname, erl_syntax:atom(binary_to_atom(TypeName))}
        ]
    ),
    Func = ?Q(
        [
            "'@name'(_@specname) ->",
            "{Spec, Opts} = _@ctor,",
            "{_@module, '@payloadtype', cmap:new(Spec, _@specname, Opts)}."
        ],
        [
            {name, erl_syntax:atom(SnakeName)},
            {specname, erl_syntax:variable(binary_to_list(TypeName))},
            {module, erl_syntax:macro(erl_syntax:variable("MODULE"))},
            {payloadtype, erl_syntax:atom(binary_to_atom(TypeName))},
            {ctor, erl_syntax:macro(erl_syntax:variable(binary_to_list(TypeName)))}
        ]
    ),
    SubConstructors =
        [
            {binary_to_atom(Name), constructor_function(binary_to_atom(Name), ObjDef)}
         || {Name, ObjDef} <- maps:to_list(maps:get(<<"definitions">>, Obj, #{}))
        ],
    [{list_to_atom(SnakeName), [Spec, Func]} | SubConstructors].

constructor_function(Name, #{<<"type">> := <<"object">>}) ->
    ?Q(
        [
            "-spec '@name'(#{atom() | binary() | string() => term()}) -> '@name'().",
            "'@name'(Val) ->",
            "{Spec, Opts} = _@ctor,",
            "cmap:new(Spec, Val, Opts)."
        ],
        [
            {name, erl_syntax:atom(Name)},
            {ctor, erl_syntax:macro(erl_syntax:variable(Name))}
        ]
    );
constructor_function(Name, #{<<"type">> := <<"string">>, <<"enum">> := _}) ->
    ?Q(
        [
            "-spec '@name'(string() | binary() | atom()) -> '@name'().",
            "'@name'(Val) ->",
            "F = _@ctor,",
            "F(Val)."
        ],
        [
            {name, erl_syntax:atom(Name)},
            {ctor, erl_syntax:macro(erl_syntax:variable(Name))}
        ]
    );
constructor_function(Name, Obj) ->
    logger:error("unexpected top level type: ~p (~p)", [Name, Obj]),
    error(crap).

snake_case(Str) ->
    snake_case(Str, []).

snake_case([], Words) ->
    string:lowercase(string:join(lists:reverse(Words), "_"));
snake_case(Str, Words) ->
    {Word, Rest} = take_word(Str),
    snake_case(Rest, [Word | Words]).

take_word([C | Rest]) ->
    take_word(Rest, [C]).
take_word([], Word) ->
    {lists:reverse(Word), []};
take_word([C1, C2 | _] = Rest, [C3 | _] = Word) when
    (C1 >= $A),
    (C1 =< $Z),
    (C2 >= $a),
    (C2 =< $z),
    (C3 >= $A),
    (C3 =< $Z)
->
    {lists:reverse(Word), Rest};
take_word([C | Rest], [LastC | _] = Word) when
    (C >= $A), (C =< $Z), (LastC >= $A), (LastC =< $Z)
->
    %% Consecutive capitals are part of the same word
    take_word(Rest, [C | Word]);
take_word([C | _] = Rest, [N | _] = Word) when
    (C >= $0),
    (C =< $9),
    (N < $0) or (N > $9)
->
    %% digits start a new word unless the follow another digit
    {lists:reverse(Word), Rest};
take_word([C | _] = Rest, Word) when (C >= $A) and (C =< $Z) ->
    {lists:reverse(Word), Rest};
take_word([C | Rest], Word) ->
    take_word(Rest, [C | Word]).

load_specs(Obj, TypeName) ->
    SubSpecs = [
        {binary_to_atom(Name), obj_spec(Name, Def)}
     || {Name, Def} <- maps:to_list(maps:get(<<"definitions">>, Obj, #{}))
    ],
    Spec = {TypeName, obj_spec(TypeName, Obj)},
    [Spec | SubSpecs].

obj_spec(Name, #{<<"type">> := <<"object">>} = Def) ->
    Required = [binary_to_atom(K) || K <- maps:get(<<"required">>, Def, [])],
    AllowExtra = maps:get(<<"additionalProperties">>, Def, true),
    ObjConstructorSpec = constructor_spec(maps:get(<<"properties">>, Def, #{})),
    Body = erl_syntax:tuple(
        [
            erl_syntax:map_expr(ObjConstructorSpec),
            ?Q("[{required, _@Required@}, {extra_keys, _@AllowExtra@}]")
        ]
    ),
    erl_syntax:attribute(erl_syntax:atom(define), [erl_syntax:variable(binary_to_list(Name)), Body]);
obj_spec(Name, #{<<"type">> := <<"string">>, <<"enum">> := _} = Def) ->
    erl_syntax:attribute(erl_syntax:atom(define), [
        erl_syntax:variable(binary_to_list(Name)),
        constructor(Def)
    ]).

constructor_spec(Properties) ->
    maps:fold(
        fun(K, V, Acc) ->
            [
                erl_syntax:map_field_assoc(
                    erl_syntax:atom(binary_to_atom(K)), constructor(V)
                )
                | Acc
            ]
        end,
        [],
        Properties
    ).

constructor(#{<<"$ref">> := <<"#/definitions/", Name/binary>>}) ->
    CName = binary_to_atom(Name),
    ?Q("fun '@cname'/1", [{cname, erl_syntax:atom(CName)}]);
constructor(#{<<"type">> := <<"string">>, <<"enum">> := EnumVals}) ->
    Vals = [binary_to_atom(Val) || Val <- EnumVals],
    ?Q("cmap:enum_(_@Vals@)");
constructor(#{<<"type">> := <<"string">>, <<"format">> := <<"date-time">>}) ->
    ?Q("fun cmap:datetime/1");
constructor(#{<<"type">> := <<"string">>} = Def) ->
    Constraints = maps:fold(
        fun
            (<<"maxLength">>, V, Acc) -> Acc#{max_length => V};
            (_, _, Acc) -> Acc
        end,
        #{},
        Def
    ),
    ?Q("cmap:string_(_@Constraints@)");
constructor(#{<<"type">> := <<"integer">>} = Def) ->
    Constraints = maps:fold(
        fun
            (<<"minimum">>, V, Acc) -> Acc#{min => trunc(V)};
            (<<"maximum">>, V, Acc) -> Acc#{max => trunc(V)};
            (_, _, Acc) -> Acc
        end,
        #{},
        Def
    ),
    ?Q("cmap:integer_(_@Constraints@)");
constructor(#{<<"type">> := <<"number">>}) ->
    ?Q("fun cmap:number/1");
constructor(#{<<"type">> := <<"boolean">>}) ->
    ?Q("fun cmap:boolean/1");
constructor(#{<<"type">> := <<"array">>} = Def) ->
    Constraints =
        maps:fold(
            fun
                (<<"minItems">>, V, Acc) ->
                    [
                        erl_syntax:map_field_assoc(
                            erl_syntax:atom(min_length), erl_syntax:integer(V)
                        )
                        | Acc
                    ];
                (<<"maxItems">>, V, Acc) ->
                    [
                        erl_syntax:map_field_assoc(
                            erl_syntax:atom(max_length), erl_syntax:integer(V)
                        )
                        | Acc
                    ];
                (<<"items">>, V, Acc) ->
                    [erl_syntax:map_field_assoc(erl_syntax:atom(items), constructor(V)) | Acc];
                (_, _, Acc) ->
                    Acc
            end,
            [],
            Def
        ),
    ?Q("cmap:list_(_@constraints)", [{constraints, erl_syntax:map_expr(Constraints)}]);
constructor(#{<<"type">> := <<"object">>} = Def) ->
    Extra = {extra_keys, maps:get(<<"additionalProperties">>, Def, true)},
    Required = {required, [binary_to_atom(K) || K <- maps:get(<<"required">>, Def, [])]},
    Spec = erl_syntax:map_expr(constructor_spec(maps:get(<<"properties">>, Def, #{}))),
    ?Q(
        [
            "fun (Input) ->",
            "cmap:new(_@spec, Input, [_@Extra@, _@Required@])",
            "end"
        ],
        [{spec, Spec}]
    );
constructor(#{<<"type">> := Type}) ->
    logger:warning("Type ~p not supported", [Type]),
    ?Q(
        "fun (_) -> error({not_supported, _@type}) end",
        [{type, erl_syntax:atom(binary_to_atom(Type))}]
    );
constructor(Def) when not is_map_key(<<"type">>, Def) ->
    ?Q("fun (X) -> X end").

load_types(Obj, TypeName) ->
    SubTypes = [
        {binary_to_atom(Name), type_def(Name, Def)}
     || {Name, Def} <- maps:to_list(maps:get(<<"definitions">>, Obj, #{}))
    ],
    [{binary_to_atom(TypeName), type_def(TypeName, Obj)} | SubTypes].

type_def(Name, #{<<"type">> := <<"object">>} = Def) ->
    Required = maps:get(<<"required">>, Def, []),
    AllowExtra = maps:get(<<"additionalProperties">>, Def, true),
    ObjMapType = object_type(maps:get(<<"properties">>, Def), Required, AllowExtra),
    DescriptionComments = description(maps:get(<<"description">>, Def, <<"">>)),
    TypeDef = merl:quote(
        "-type '" ++ binary_to_list(Name) ++ "'() :: " ++ erl_prettypr:format(ObjMapType) ++ "."
    ),
    DescriptionComments ++ [TypeDef];
type_def(Name, #{<<"type">> := <<"string">>, <<"enum">> := EnumVals} = Def) ->
    Enum = erl_syntax:type_union([erl_syntax:atom(binary_to_atom(Val)) || Val <- EnumVals]),
    DescriptionComments = description(maps:get(<<"description">>, Def, <<"">>)),
    TypeDef = merl:quote(
        "-type '" ++ binary_to_list(Name) ++ "'() :: " ++ erl_prettypr:format(Enum) ++ "."
    ),
    DescriptionComments ++ [TypeDef];
type_def(Name, Def) ->
    [
        merl:quote(
            "-type '" ++ binary_to_list(Name) ++ "'() :: " ++
                erl_prettypr:format(property_type(Def)) ++ "."
        )
    ].

description(<<"">>) ->
    [];
description(Descr) ->
    [
        erl_syntax:comment(
            ["% @doc"] ++
                lists:concat(
                    [
                        par(Line)
                     || Line <- string:split(
                            string:trim(binary_to_list(Descr)), "\r\n", all
                        )
                    ]
                ) ++
                ["% @end"]
        )
    ].

par("") ->
    ["% "];
par(Line) ->
    ["% " ++ L || L <- string:split(prettypr:format(prettypr:text_par(Line)), "\n", all)].

object_type(Properties, Required, AllowExtra) ->
    PropertyTypes =
        maps:fold(
            fun(K, V, Acc) ->
                case lists:member(K, Required) of
                    true ->
                        [
                            erl_syntax:map_type_exact(
                                erl_syntax:atom(binary_to_atom(K)),
                                property_type(V)
                            )
                            | Acc
                        ];
                    false ->
                        [
                            erl_syntax:map_type_assoc(
                                erl_syntax:atom(binary_to_atom(K)),
                                property_type(V)
                            )
                            | Acc
                        ]
                end
            end,
            if
                AllowExtra ->
                    [
                        erl_syntax:map_type_assoc(
                            erl_syntax:type_application(
                                erl_syntax:atom(binary), []
                            ),
                            erl_syntax:type_application(erl_syntax:atom(term), [])
                        )
                    ];
                not AllowExtra ->
                    []
            end,
            Properties
        ),
    erl_syntax:map_type(PropertyTypes).

property_type(#{<<"type">> := <<"string">>, <<"format">> := <<"date-time">>}) ->
    erl_syntax:type_application(
        erl_syntax:module_qualifier(
            erl_syntax:atom(calendar),
            erl_syntax:atom(datetime)
        ),
        []
    );
property_type(#{<<"type">> := <<"array">>, <<"items">> := Items}) ->
    erl_syntax:type_application(
        erl_syntax:atom(list), [property_type(Items)]
    );
property_type(#{<<"type">> := <<"object">>} = Def) ->
    object_type(
        maps:get(<<"properties">>, Def, #{}),
        maps:get(<<"required">>, Def, []),
        maps:get(<<"additionalProperties">>, Def, true)
    );
property_type(#{<<"type">> := Type}) ->
    erl_syntax:type_application(
        erl_syntax:atom(type_name(Type)), []
    );
property_type(#{<<"$ref">> := <<"#/definitions/", Type/binary>>}) ->
    erl_syntax:type_application(
        erl_syntax:atom(binary_to_atom(Type)), []
    );
property_type(_) ->
    erl_syntax:type_application(erl_syntax:atom(term), []).

%% TODO make sure this handles all types correctly
type_name(Type) ->
    binary_to_atom(Type).
