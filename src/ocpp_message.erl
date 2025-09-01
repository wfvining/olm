-module(ocpp_message).
-moduledoc """
Terms represnting OCPP messages. This module provides a thing wrapper
around version-specific modules.
""".

-export([version/1, module/1, encode/1, decode/4, type/1, action/1, get/2]).

-export_type([message/0, raw_message/0]).

-opaque raw_message() ::
    {raw_message, string(), request | response, #{binary() => json:decode_value()}}.
-type message() ::
    ocpp_message_1_6:message()
    | ocpp_message_2_0_1:message()
    | ocpp_message_2_1:message().

-define(MODULES, #{
    '1.6' => ocpp_message_1_6,
    '2.0.1' => ocpp_message_2_0_1,
    '2.1' => ocpp_message_2_1
}).

-define(VERSIONS, #{
    ocpp_message_1_6 => '1.6',
    ocpp_message_2_0_1 => '2.0.1',
    ocpp_message_2_1 => '2.1'
}).

-doc """
Return the OCPP version used to create a message.
""".
-spec version(message()) -> ocpp:version().
version({MsgModule, _, _}) ->
    maps:get(MsgModule, ?VERSIONS).

-doc """
Return the module for a specific OCPP version.
""".
-spec module(ocpp:version()) -> module().
module(Version) ->
    maps:get(Version, ?MODULES).

-doc """
Encode a message payload as JSON.
""".
-spec encode(message()) -> binary().
encode({_, _, Payload}) ->
    iolist_to_binary(cmap:to_json(Payload)).

-doc """
Decode a message payload.
""".
-spec decode(
    Version :: ocpp:version(),
    PayloadType :: binary(),
    Direction :: request | response,
    Payload :: json:decode_value()
) ->
    {ok, message()}
    | {error, Reason :: term()}.
%% decode(_, _, _, Payload) when not is_map(Payload) ->
%%     {error, bad_payload};
decode('1.6', PayloadType, Direction, Payload) ->
    ocpp_message_1_6:decode(PayloadType, Direction, Payload);
decode('2.0.1', PayloadType, Direction, Payload) ->
    ocpp_message_2_0_1:decode(PayloadType, Direction, Payload);
decode('2.1', PayloadType, Direction, Payload) ->
    ocpp_message_2_1:decode(PayloadType, Direction, Payload).

-spec type(message()) ->
    ocpp_message_1_6:payload_type()
    | ocpp_message_2_0_1:payload_type()
    | ocpp_message_2_1:payload_type().
type({_Version, Type, _}) ->
    Type.

-doc """
Return the action name for the message (i.e. the message type without
the 'Request' or 'Response' suffix, if the suffix exists).
""".
-spec action(message()) -> binary().
action({_Version, Type, _}) ->
    TypeStr = atom_to_binary(Type),
    case re:run(TypeStr, ~S"^(?<action>.*)(?:Request|Response)$", [{capture, [action], binary}]) of
        nomatch ->
            TypeStr;
        {match, [Action]} ->
            Action
    end.

-doc """
Get the value of a message property.
""".
-spec get(Key :: atom() | binary(), Message :: ocpp_message:message()) -> term().
get(Key, {_Version, _Type, Message}) ->
    maps:get(Key, Message).
