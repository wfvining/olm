-module(ocpp_message).
-moduledoc """
Terms represnting OCPP messages. This module provides a thing wrapper
around version-specific modules.
""".

-export([version/1, encode/1, decode/4, decode_partial/3, decode_finish/2]).

-export_type([message/0, raw_message/0]).

-opaque raw_message() ::
    {raw_message, string(), request | response, #{binary() => json:decode_value()}}.
-type message() ::
    ocpp_message_1_6:message()
    | ocpp_message_2_0_1:message()
    | ocpp_message_2_1:message().

-define(MODULES, #{
    "1.6" => ocpp_message_1_6,
    "2.0.1" => ocpp_message_2_0_1,
    "2.1" => ocpp_message_2_1
}).

-doc """
Returns the version-specific message module. If the version is not
supported the call fails with reason `{unsupported, Version}`.
""".
-spec version(string() | binary()) -> module().
version(Version) when is_binary(Version) ->
    version(binary_to_list(Version));
version(Version) ->
    try
        maps:get(Version, ?MODULES)
    catch
        error:{badkey, Version} ->
            error({unsupported, Version})
    end.

-doc """
Encode a message payload as JSON.
""".
encode({_, _, Payload}) ->
    cmap:to_json(Payload).

-doc """
Decode a message payload.
""".
-spec decode(
    Version :: string(),
    PayloadType ::
        ocpp_message_1_6:payload_type()
        | ocpp_message_2_0_1:payload_type()
        | ocpp_message_2_1:payload_type(),
    Direction :: request | response,
    Payload :: json:decode_value()
) ->
    {ok,
        ocpp_message_2_0_1:message()
        | ocpp_message_2_1:message()
        | ocpp_message_1_6:message()}
    | {error, Reason :: term()}.
decode("1.6", PayloadType, Direction, Payload) ->
    ocpp_message_1_6:decode(PayloadType, Direction, Payload);
decode("2.0.1", PayloadType, Direction, Payload) ->
    ocpp_message_2_0_1:decode(PayloadType, Direction, Payload);
decode("2.1", PayloadType, Direction, Payload) ->
    ocpp_message_2_1:decode(PayloadType, Direction, Payload).

-spec decode_partial(string(), request | response, #{binary() => json:decode_value()}) ->
    raw_message().
decode_partial(Version, Direction, Payload) ->
    {raw_message, Version, Direction, Payload}.

-spec decode_finish(
    PayloadType ::
        ocpp_message_1_6:payload_type()
        | ocpp_message_2_0_1:payload_type()
        | ocpp_message_2_1:payload_type(),
    raw_message()
) ->
    {ok, ocpp_message_1_6:message() | ocpp_message_2_0_1:message() | ocpp_message_2_1:message()}
    | {error, Reason :: term()}.
decode_finish(PayloadType, {raw_message, Version, Direction, Payload}) ->
    decode(Version, PayloadType, Direction, Payload).
