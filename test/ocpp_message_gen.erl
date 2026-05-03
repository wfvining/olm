-module(ocpp_message_gen).
%% eqwalizer struggles with proper macros. It may not be wrong, but it is definitely unhelpful
-eqwalizer(ignore).

-include_lib("proper/include/proper.hrl").

-export([pdu/2, pdu/3, message/1, message/2, message/3, request/1]).

%% 1. load schemas
%% 2. load overrides
%% 3. apply overrides
%% 4. store the term
%% 5. generate object from spec
%%    - primitives
%%    - arrays
%%    - objects (required keys, optional keys)

-doc """
Generator for OCPP messages. Fields may be set to specific values (or
generators) by passing the option `[{override, #{Property =>
ValueOrGenerator}}]`. Deep properties can be specified with
`#{Property => #{SubProperty => Value}}`. To generate a message
without specific properties use `{without, [Property]}` (NOTE only
removing top level properties is supported).
""".
pdu(Version, MessageType) ->
    pdu(Version, MessageType, []).
pdu(Version, MessageType, Options) ->
    Schema = load_schema(Version, MessageType),
    ?LET(
        Obj,
        gen_cmap(Schema),
        ?LET(
            M,
            apply_custom(Obj, proplists:get_value(override, Options, #{})),
            remove_properties(
                proplists:get_value(without, Options, []),
                M
            )
        )
    ).

message(Version) ->
    %% exists in all versions
    load_schema(Version, ~"BootNotificationRequest"),
    MTypes = persistent_term:get({?MODULE, Version}),
    oneof([message(Version, MessageType) || MessageType <- maps:keys(MTypes)]).
message(Version, MessageType) ->
    message(Version, MessageType, []).
message(Version, MessageType, Options) ->
    ?LET(
        PDU,
        pdu(Version, MessageType, Options),
        begin
            {ok, Message} = ocpp_message:new(Version, MessageType, PDU),
            Message
        end
    ).

request(Version) ->
    load_schema(Version, ~"BootNotificationRequest"),
    MTypes = persistent_term:get({?MODULE, Version}),
    oneof([message(Version, MessageType) || MessageType <- maps:keys(MTypes), 
                  binary:match(MessageType, ~"Request", [{scope, {byte_size(MessageType), -7}}]) =/= nomatch]).

%% XXX This just removes keys from top level object. Full support for
%% recursively descending into arrays or for specifying properties in
%% sub-objects still needs to be added.
remove_properties([], M) ->
    M;
remove_properties([Prop | Rest], M) ->
    remove_properties(Rest, maps:remove(Prop, M)).

apply_custom(Obj, CustomGens) ->
    %% XXX This doesn't handle everything, in particular cases related to arrays.
    ?LET(
        Overrides,
        gen_custom(CustomGens),
        maps:merge_with(
            fun
                F(_, M1, M2) when is_map(M1), is_map(M2) -> maps:merge_with(F, M1, M2);
                F(_, A1, A2) when is_list(A1), is_map(A2) ->
                    [maps:merge_with(F, X, A2) || X <- A1];
                F(_, _, V) ->
                    V
            end,
            Obj,
            Overrides
        )
    ).

gen_custom(M) when is_map(M) ->
    ?LET(Xs, [{K, gen_custom(V)} || K := V <- M], maps:from_list(Xs));
gen_custom(G) ->
    G.

load_schema(Version, SchemaName) ->
    try
        Specs = persistent_term:get({?MODULE, Version}),
        maps:get(SchemaName, Specs)
    catch
        error:badarg ->
            try_load_schemas(Version),
            load_schema(Version, SchemaName)
    end.

try_load_schemas(Version) ->
    SchemaFile = filename:join([code:priv_dir(ocpp), "messages", atom_to_list(Version), "specs"]),
    OverrideFile = filename:join([
        code:priv_dir(ocpp), "messages", atom_to_list(Version), "overrides"
    ]),
    maybe
        {ok, Specs} ?= file:consult(SchemaFile),
        {ok, Overrides} ?= file:consult(OverrideFile),
        OverrideMap = maps:from_list(Overrides),
        SpecMap = #{Name => merge_overrides(Spec, OverrideMap) || {Name, Spec} <- Specs},
        persistent_term:put({?MODULE, Version}, SpecMap)
    else
        Err ->
            error({load_schemas_failed, SchemaFile, OverrideFile, Err})
    end.

merge_overrides(#{defs := Defs} = Constraints, Overrides) ->
    Constraints#{defs => maps:merge_with(fun merge_overrides/3, Defs, Overrides)}.
merge_overrides(_Name, {object, Constraints}, Overrides) ->
    {object, maps:merge_with(fun merge_overrides/3, Constraints, Overrides)};
merge_overrides(_Name, {Type, Constraints}, {Type, Overrides}) when is_map(Constraints) ->
    {Type, maps:merge_with(fun merge_overrides/3, Constraints, Overrides)};
merge_overrides(_Name, M1, M2) when is_map(M1), is_map(M2) ->
    maps:merge_with(fun merge_overrides/3, M1, M2);
merge_overrides(_Name, _, V) ->
    V.

subset(List) ->
    ?LET(P, float(0.0, 1.0), [Gen || Gen <- List, rand:uniform() =< P]).

gen_cmap(Spec) ->
    gen_cmap({object, Spec}, maps:get(defs, Spec, #{})).

gen_cmap({object, #{properties := Properties} = Constraints}, Defs) ->
    RequiredProperties = maps:get(required, Constraints, []),
    OptionalProperties = maps:keys(Properties) -- RequiredProperties,
    ?LET(
        {Required, Optional},
        {
            [{P, gen_cmap(maps:get(P, Properties), Defs)} || P <- RequiredProperties],
            ?SIZED(
                S,
                ?SHRINK(
                    frequency([
                        {1,
                            subset([
                                {P, resize(S div 10, gen_cmap(maps:get(P, Properties), Defs))}
                             || P <- OptionalProperties
                            ])},
                        {99,
                            subset([
                                {P, resize(S div 10, gen_cmap(maps:get(P, Properties), Defs))}
                             || P <- OptionalProperties, P =/= 'customData'
                            ])}
                    ]),
                    [
                        subset([
                            {P, resize(S div 100, gen_cmap(maps:get(P, Properties), Defs))}
                         || P <- OptionalProperties, P =/= 'customData'
                        ]),
                        []
                    ]
                )
            )
        },
        maps:from_list(Required ++ Optional)
    );
gen_cmap({array, #{items := ISpec} = Constraints}, Defs) ->
    ?LET(
        Len,
        ?SIZED(
            S,
            resize(
                S div 8,
                integer(
                    maps:get(min_items, Constraints, 0),
                    maps:get(max_items, Constraints, inf)
                )
            )
        ),
        vector(Len, gen_cmap(ISpec, Defs))
    );
gen_cmap({integer, Constraints}, _) ->
    Lims = to_integer_limits(Constraints),
    integer(maps:get(min, Lims, inf), maps:get(max, Lims, inf));
gen_cmap({float, Constraints}, _) ->
    Lims = to_float_limits(Constraints),
    float(maps:get(min, Lims, inf), maps:get(max, Lims, inf));
gen_cmap({number, Constraints}, Defs) ->
    oneof([
        gen_cmap({integer, to_integer_limits(Constraints)}, Defs),
        gen_cmap({float, to_float_limits(Constraints)}, Defs)
    ]);
gen_cmap({string, Constraints}, _) ->
    utf8(maps:get(max_length, Constraints, inf));
gen_cmap({enum, Choices}, _) ->
    oneof(Choices);
gen_cmap(any, _) ->
    %% just make sure this is encodable as a JSON term
    ?LET(M, list({atom(), oneof([utf8(), atom(), number(), boolean()])}), maps:from_list(M));
gen_cmap(boolean, _) ->
    boolean();
gen_cmap(datetime, _) ->
    prop_datetime:datetime();
gen_cmap({ref, Name}, Defs) ->
    gen_cmap(maps:get(Name, Defs), Defs).

to_integer_limits(Constraints) ->
    maps:map(
        fun
            (min, Val) -> trunc(math:ceil(Val));
            (max, Val) -> trunc(math:floor(Val))
        end,
        Constraints
    ).
to_float_limits(Constraints) ->
    #{K => erlang:float(V) || K := V <- Constraints}.
