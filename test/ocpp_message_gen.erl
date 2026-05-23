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
        gen_cmap(Schema, Options),
        maps:without(
            proplists:get_value(without, Options, []),
            Obj
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
    oneof([
        message(Version, MessageType)
     || MessageType <- maps:keys(MTypes),
        binary:match(MessageType, ~"Request", [{scope, {byte_size(MessageType), -7}}]) =/= nomatch
    ]).

%% XXX This just removes keys from top level object. Full support for
%% recursively descending into arrays or for specifying properties in
%% sub-objects still needs to be added.
remove_properties([], M) ->
    M;
remove_properties([Prop | Rest], M) ->
    remove_properties(Rest, maps:remove(Prop, M)).

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

gen_cmap(Spec, Options) ->
    Overrides = proplists:get_value(override, Options, undefined),
    gen_cmap({object, Spec}, Overrides, maps:get(defs, Spec, #{})).

gen_cmap({object, #{properties := Properties} = Constraints}, undefined, Defs) ->
    RequiredProperties = maps:get(required, Constraints, []),
    OptionalProperties = maps:keys(Properties) -- RequiredProperties,
    ?LET(
        {Required, Optional},
        {
            [{P, gen_cmap(maps:get(P, Properties), undefined, Defs)} || P <- RequiredProperties],
            optional_properties(maps:with(OptionalProperties, Properties), Defs)
        },
        maps:from_list(Required ++ Optional)
    );
gen_cmap({object, #{properties := Properties} = Constraints}, Overrides, Defs) when
    is_map(Overrides)
->
    Overridden = maps:keys(Overrides),
    RequiredProperties = lists:uniq(maps:get(required, Constraints, []) ++ Overridden),
    OptionalProperties = maps:keys(Properties) -- RequiredProperties,
    ?LET(
        {Required, Optional},
        {
            [
                {P, gen_cmap(maps:get(P, Properties), maps:get(P, Overrides, undefined), Defs)}
             || P <- RequiredProperties
            ],
            optional_properties(maps:with(OptionalProperties, Properties), Defs)
        },
        maps:from_list(Required ++ Optional)
    );
gen_cmap({array, #{items := ISpec} = Constraints}, undefined, Defs) ->
    ?LET(
        Len,
        ?SIZED(
            Size,
            resize(
                Size div 1000,
                integer(
                    maps:get(min_items, Constraints, 0),
                    maps:get(max_items, Constraints, inf)
                )
            )
        ),
        vector(Len, gen_cmap(ISpec, undefined, Defs))
    );
gen_cmap({array, #{items := ISpec}}, Overrides, Defs) when is_list(Overrides) ->
    [gen_cmap(ISpec, Override, Defs) || Override <- Overrides];
gen_cmap({integer, Constraints}, undefined, _) ->
    Lims = to_integer_limits(Constraints),
    integer(maps:get(min, Lims, inf), maps:get(max, Lims, inf));
gen_cmap({float, Constraints}, undefined, _) ->
    Lims = to_float_limits(Constraints),
    float(maps:get(min, Lims, inf), maps:get(max, Lims, inf));
gen_cmap({number, Constraints}, undefined, Defs) ->
    oneof([
        gen_cmap({integer, to_integer_limits(Constraints)}, undefined, Defs),
        gen_cmap({float, to_float_limits(Constraints)}, undefined, Defs)
    ]);
gen_cmap({string, Constraints}, undefined, _) ->
    utf8(maps:get(max_length, Constraints, inf));
gen_cmap({enum, Choices}, undefined, _) ->
    oneof(Choices);
gen_cmap(any, undefined, _) ->
    %% just make sure this is encodable as a JSON term
    ?LET(M, list({atom(), oneof([utf8(), atom(), number(), boolean()])}), maps:from_list(M));
gen_cmap(boolean, undefined, _) ->
    boolean();
gen_cmap(datetime, undefined, _) ->
    prop_datetime:datetime();
gen_cmap({ref, Name}, Overrides, Defs) ->
    gen_cmap(maps:get(Name, Defs), Overrides, Defs);
gen_cmap(_, Override, _) when Override =/= undefined ->
    %% catch-all clause handles proper generators being assigned as an
    %% override as well as overrides for primitive values.
    Override.

optional_properties(Opts, Defs) ->
    ?SHRINK(
        frequency(
            [
                {1, subset([{P, gen_cmap(Spec, undefined, Defs)} || P := Spec <- Opts])},
                {10,
                    subset([
                        {P, gen_cmap(Spec, undefined, Defs)}
                     || P := Spec <- Opts,
                        P =/= 'customData'
                    ])}
            ]
        ),
        [
            []
        ]
    ).

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
