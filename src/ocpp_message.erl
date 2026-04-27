-module(ocpp_message).

-export([decode/3, new/3, encode/1, action/1, type/1, get/2, get/3, unload/1]).

-export_type([message/0]).

-record(ocppmessage, {version :: ocpp:version(), payloadtype :: binary(), payload :: map()}).

-opaque message() :: #ocppmessage{}.

-doc """
Decode a map from a `json:decode_value()` to an OCPP message
representation.
""".
-spec decode(ocpp:version(), binary(), json:decode_value()) ->
    {ok, message()} | {error, Reason :: term()}.
decode(Version, MessageType, Payload) when is_map(Payload) ->
    Schema = lookup_schema(Version, MessageType),
    case cmap:new(Payload, Schema) of
        {ok, Message} ->
            {ok, #ocppmessage{version = Version, payloadtype = MessageType, payload = Message}};
        {error, _} = Error ->
            Error
    end.

new(Version, MessageType, Payload) ->
    decode(Version, MessageType, Payload).

-doc """
Encode a message as a JSON binary.
""".
-spec encode(message()) -> binary().
encode(#ocppmessage{payload = Message}) ->
    iolist_to_binary(cmap:to_json(Message)).

-doc """
Return the action name for the message (i.e. the message type without
the 'Request' or 'Response' suffix, if the suffix exists).
""".
-spec action(message()) -> binary().
action(#ocppmessage{payloadtype = Type}) ->
    case re:run(Type, ~S"^(?<action>.*)(?:Request|Response)$", [{capture, [action], binary}]) of
        nomatch ->
            %% Must pass through non-matching types for new messages in OCPP 2.1
            Type;
        {match, [Action]} ->
            Action
    end.

-doc """
Return the message type (i.e. the action name suffixed with "Request" or "Response").
""".
-spec type(message()) -> binary().
type(#ocppmessage{payloadtype = Type}) ->
    Type.

-doc """
Retrieve the value associated with `Key`. If the key is not present
the call fails with an error `{badkey, Key}`.
""".
-spec get(Key :: atom() | binary(), Message :: message()) -> cmap:value().
get(Key, #ocppmessage{payload = Message}) ->
    maps:get(Key, Message).

-spec get(Key :: atom() | binary(), Message :: message(), Default) -> cmap:value() | Default.
get(Key, #ocppmessage{payload = Message}, Default) ->
    maps:get(Key, Message, Default).

-doc """
Unload the schemas for `Version`. They will be loaded again the next
time they are required.
""".
-spec unload(ocpp:version()) -> ok.
unload(Version) ->
    persistent_term:erase({?MODULE, Version}),
    ok.

lookup_schema(Version, SchemaName) ->
    try persistent_term:get({?MODULE, Version}) of
        #{SchemaName := Spec} ->
            Spec;
        _ ->
            error({unknown_message, SchemaName})
    catch
        error:badarg ->
            load_schemas_and_retry(Version, SchemaName)
    end.

load_schemas_and_retry(Version, SchemaName) ->
    Priv = code:priv_dir(ocpp),
    SpecPath = filename:join([Priv, "messages", atom_to_binary(Version), "specs"]),
    case file:consult(SpecPath) of
        {ok, SpecList} ->
            Specs = load_overrides(filename:dirname(SpecPath), maps:from_list(SpecList)),
            persistent_term:put({?MODULE, Version}, Specs),
            lookup_schema(Version, SchemaName);
        {error, {_, _, _} = Reason} ->
            error({invalid_message_specs, {file:format_error(Reason), Version, SpecPath}});
        {error, _Reason} ->
            error({unsupported_version, Version})
    end.

load_overrides(SpecDir, Schemas) ->
    case file:consult(filename:join([SpecDir, "overrides"])) of
        {ok, Overrides} ->
            apply_overrides(maps:from_list(Overrides), Schemas);
        {error, _} ->
            Schemas
    end.

apply_overrides(Overrides, Schemas) ->
    #{SchemaName => do_apply_overrides(Overrides, Spec) || SchemaName := Spec <- Schemas}.

merge_specs(_Name, {object, Constraints}, Overrides) ->
    {object, maps:merge_with(fun merge_specs/3, Constraints, Overrides)};
merge_specs(_Name, {Type, Constraints}, {Type, Overrides}) ->
    {Type, maps:merge(Constraints, Overrides)};
merge_specs(_Name, Val1, Val2) when is_map(Val1), is_map(Val2) ->
    maps:merge_with(fun merge_specs/3, Val1, Val2);
merge_specs(_Name, _, V) ->
    V.

do_apply_overrides(Overrides, #{defs := Defs} = OSpec) ->
    OSpec#{defs => maps:merge_with(fun merge_specs/3, Defs, maps:with(maps:keys(Defs), Overrides))}.
