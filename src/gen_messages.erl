-module(gen_messages).

-include_lib("syntax_tools/include/merl.hrl").

-export([main/1]).

main(Args) ->
    argparse:run(Args, cli(), #{progname => gen_messages}).

cli() ->
    #{
        arguments =>
            [
                #{name => schemadir, help => "Path to directory containing JSON schemas."},
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

gen(Args = #{schemadir := Path}) ->
    {ok, Files} = file:list_dir(Path),
    Schemas = [filename:join(Path, File) || File <- Files, filename:extension(File) =:= ".json"],
    Specs = [load_schema(SchemaFile) || SchemaFile <- Schemas],
    OutDir = maps:get(outdir, Args, "."),
    file:write_file(
        filename:join([OutDir, "specs"]),
        [io_lib:format("~p.~n", [{SpecName, Spec}]) || {SpecName, Spec} <- Specs]
    ).

load_schema(SchemaFile) ->
    {ok, Schema} = file:read_file(SchemaFile),
    Obj = json:decode(Schema),
    OID =
        if
            is_map_key(~"$id", Obj) ->
                maps:get(~"$id", Obj);
            is_map_key(~"id", Obj) ->
                maps:get(~"id", Obj)
        end,
    Defs = maps:get(~"definitions", Obj, #{}),
    TypeName = type_id(Obj),
    BaseSpec = load_object_spec(Obj),
    Spec = BaseSpec#{
        id => OID,
        defs =>
            #{
                binary_to_atom(K) => load_spec(V)
             || K := V <- Defs
            }
    },
    {TypeName, Spec}.

load_spec(#{~"type" := ~"object"} = Spec) ->
    {object, load_object_spec(Spec)};
load_spec(#{~"type" := ~"array", ~"items" := ItemSpec} = Spec) ->
    LengthConstraints = array_length_constraints(Spec),
    {array, LengthConstraints#{items => load_spec(ItemSpec)}};
load_spec(#{~"$ref" := <<"#/definitions/", Ref/binary>>}) ->
    {ref, binary_to_atom(Ref)};
load_spec(#{~"type" := Type} = Spec) when
    Type =:= ~"integer";
    Type =:= ~"float";
    Type =:= ~"number"
->
    {binary_to_atom(Type), coerce_type(Type, number_constraints(Spec))};
load_spec(#{~"type" := ~"string", ~"enum" := Enum}) ->
    {enum, [binary_to_atom(Val) || Val <- Enum]};
load_spec(#{~"type" := ~"string", ~"maxLength" := MaxLen}) ->
    {string, #{max_length => MaxLen}};
load_spec(#{~"type" := ~"string", ~"format" := ~"date-time"}) ->
    datetime;
load_spec(#{~"type" := ~"string"}) ->
    {string, #{}};
load_spec(#{~"type" := ~"boolean"}) ->
    boolean;
load_spec(Spec) when not is_map_key(~"type", Spec) ->
    any.

coerce_type(~"integer", Limits) ->
    maps:map(
        fun
            (min, Min) when is_float(Min) -> trunc(math:ceil(Min));
            (max, Max) when is_float(Max) -> trunc(math:floor(Max));
            (_, X) -> X
        end,
        Limits
    );
coerce_type(~"float", Limits) ->
    #{K => float(V) || K := V <- Limits};
coerce_type(~"number", Limits) ->
    Limits.

number_constraints(#{~"minimum" := Min, ~"maximum" := Max}) ->
    #{min => Min, max => Max};
number_constraints(#{~"minimum" := Min}) ->
    #{min => Min};
number_constraints(#{~"maximum" := Max}) ->
    #{max => Max};
number_constraints(_) ->
    #{}.

array_length_constraints(#{~"minItems" := MinItems, ~"maxItems" := MaxItems}) ->
    #{min_items => MinItems, max_items => MaxItems};
array_length_constraints(#{~"minItems" := MinItems}) ->
    #{min_items => MinItems};
array_length_constraints(#{~"maxItems" := MaxItems}) ->
    #{max_items => MaxItems};
array_length_constraints(_) ->
    #{}.

load_object_spec(Obj) ->
    #{
        properties =>
            #{binary_to_atom(K) => load_spec(V) || K := V <- maps:get(~"properties", Obj)},
        required => [binary_to_atom(Property) || Property <- maps:get(~"required", Obj, [])],
        doc =>
            #{
                binary_to_atom(Property) => string:trim(Description)
             || Property := #{~"description" := Description} <- maps:get(~"properties", Obj)
            },
        additional_properties => maps:get(~"additionalProperties", Obj, true)
    }.

type_id(#{~"id" := ID}) ->
    lists:last(string:split(ID, ":", all));
type_id(#{~"$id" := ID}) ->
    lists:last(string:split(ID, ":", all)).
