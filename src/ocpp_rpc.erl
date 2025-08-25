-module(ocpp_rpc).

-export([decode/3]).
-export([id/1, error_type/1, error_code/1, error_description/1]).
-export([payload/1]).

-export_type([error_code/0, rpctype/0]).
-export_type([call/0, callresult/0, callerror/0, callresulterror/0, send/0]).

-type error_code() ::
    'FormatViolation'
    | 'GenericError'
    | 'InternalError'
    | 'MessageTypeNotSupported'
    | 'NotImplemented'
    | 'NotSupported'
    | 'OccurenceConstraintViolation'
    | 'PropertyConstraintViolation'
    | 'ProtocolError'
    | 'RpcFrameworkError'
    | 'SecurityError'
    | 'TypeConstraintViolation'.

-record(callerror, {
    id = <<"-1">> :: binary(),
    code :: error_code(),
    description = <<"">> :: binary(),
    data = #{} :: #{binary() => json:decode_value()}
}).

-record(callresulterror, {
    id = <<"-1">> :: binary(),
    code :: error_code(),
    description = <<"">> :: binary(),
    data = #{} :: #{binary() => json:decode_value()}
}).

-record(call, {id :: binary(), action :: binary(), payload :: ocpp_message:message()}).
-record(send, {id :: binary(), action :: binary(), payload :: ocpp_message:message()}).
-record(callresult, {id :: binary(), payload :: ocpp_message:message()}).

-opaque callerror() :: #callerror{}.
-opaque callresulterror() :: #callresulterror{}.
-opaque call() :: #call{}.
-opaque send() :: #send{}.
-opaque callresult() :: #callresult{}.

-type rpctype() :: call | send | callresult | callerror | callresulterror.

-doc """
Decode an RPC message. If decoding is succesful a tuple `{ok, RPC}` is
returned where `RPC` is a tuple containint the message type and the
decoded message is returned. If decoding fails `{error, RPCError}` is
returned where `RPCError` is a tuple containting the error type and an
appropriate `callerror` or `callresulterror` is returned.

If an error occurs while decoding an error message for which there is
no appropriate error response `{error, {error, Reason}}` is returned.
This indicates that the message should be dropped while providing
information, in the form of a `t:callerror()` or a
`t:callresulterror()` about why the decoding failed.
""".
-spec decode(ocpp:version(), binary(), [{expected, binary()}]) ->
    {ok, {call, call()}}
    | {ok, {callresult, callresult()}}
    | {ok, {callresulterror, callresulterror()}}
    | {ok, {callerror, callerror()}}
    | {ok, {send, send()}}
    | {error, {callresulterror, callresulterror()}}
    | {error, {callerror, callerror()}}
    | {error, {error, callerror() | callresulterror()}}.
decode(Version, RPCBinary, Options) ->
    try json:decode(RPCBinary) of
        [2, ID, Action, Payload] ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_request(call, Version, ID, Action, Payload);
                {error, _} ->
                    {error,
                        {callerror, #callerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [2 | Rest] ->
            ID = maybe_message_id(Rest),
            {error,
                {callerror, #callerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALL">>
                }}};
        [3, ID, Payload] ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_result(
                        Version, ID, Payload, proplists:get_value(expected, Options)
                    );
                {error, _} when Version =:= '2.1' ->
                    {error,
                        {callresulterror, #callresulterror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}};
                {error, _} ->
                    {error,
                        {error, #callerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [3 | Rest] when Version =:= '2.1' ->
            ID = maybe_message_id(Rest),
            {error,
                {callresulterror, #callresulterror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLRESULT">>
                }}};
        [3 | Rest] ->
            ID = maybe_message_id(Rest),
            {error,
                {error, #callerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLRESULT">>
                }}};
        [4, ID, ErrorCode, Description, Data] ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_error(callerror, ID, ErrorCode, Description, Data);
                {error, _} ->
                    {error,
                        {error, #callerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [4 | Rest] ->
            ID = maybe_message_id(Rest),
            {error,
                {error, #callerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLERROR">>
                }}};
        [5, ID, ErrorCode, Description, Data] when Version =:= '2.1' ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_error(callresulterror, ID, ErrorCode, Description, Data);
                {error, _} ->
                    {error,
                        {error, #callerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [5 | Rest] when Version =:= '2.1' ->
            ID = maybe_message_id(Rest),
            {error,
                {error, #callerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLRESULTERROR">>
                }}};
        [5 | Rest] ->
            Vsn = atom_to_binary(Version),
            ID = maybe_message_id(Rest),
            {error,
                {callerror, #callerror{
                    code = 'MessageTypeNotSupported',
                    id = ID,
                    description = <<"CALLRESULTERROR not supported by OCPP version ", Vsn/binary>>
                }}};
        [6, ID, Action, Payload] when Version =:= '2.1' ->
            maybe
                {ok, ID} ?= validate_message_id(ID),
                {ok, _} ?= decode_request(send, Version, ID, Action, Payload)
            else
                {error, <<"-1">>} ->
                    {error,
                        {error, #callerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}};
                %% have to filter the resulting error since for a send
                %% they are dropped. returning them as an `error`
                %% means we signal that the error is fatal (i.e. the
                %% message should be dropped) but still allow it to be
                %% logged if desired.
                {error, {callerror, Reason}} ->
                    {error, {error, Reason}}
            end;
        [6 | Rest] when Version =:= '2.1' ->
            ID = maybe_message_id(Rest),
            {error,
                {error, #callerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid SEND">>
                }}};
        [6 | Rest] ->
            ID = maybe_message_id(Rest),
            Vsn = atom_to_binary(Version),
            {error,
                {callerror, #callerror{
                    code = 'MessageTypeNotSupported',
                    id = ID,
                    description = <<"SEND not supported by OCPP version ", Vsn/binary>>
                }}};
        [TypeID | Rest] when is_integer(TypeID) ->
            ID = maybe_message_id(Rest),
            TID = integer_to_binary(TypeID),
            Vsn = atom_to_binary(Version),
            {error,
                {callerror, #callerror{
                    code = 'MessageTypeNotSupported',
                    id = ID,
                    description =
                        <<"Message type ", TID/binary, " not supported by OCPP version ",
                            Vsn/binary>>
                }}};
        _ ->
            {error,
                {error, #callerror{
                    code = 'RpcFrameworkError',
                    description = <<"Invalid RPC wrapper">>
                }}}
    catch
        error:_ ->
            {error,
                {error, #callerror{
                    code = 'RpcFrameworkError', description = <<"Invalid JSON">>
                }}}
    end.

maybe_message_id([]) ->
    <<"-1">>;
maybe_message_id([ID | _]) ->
    element(2, validate_message_id(ID)).

validate_message_id(ID) when is_binary(ID) ->
    case string:length(ID) of
        N when N > 36 ->
            {error, <<"-1">>};
        _ ->
            {ok, ID}
    end;
validate_message_id(_) ->
    {error, <<"-1">>}.

decode_error(Type, ID, Code, Description, Data) when
    is_binary(Code), is_binary(Description), is_map(Data)
->
    try binary_to_existing_atom(Code) of
        ErrorCode ->
            {ok, {Type, {Type, ID, ErrorCode, Description, Data}}}
    catch
        error:badarg ->
            {error,
                {error, #callerror{
                    code = 'RpcFrameworkError',
                    id = ID,
                    description = <<"Unknown error code">>,
                    data = #{<<"errorCode">> => Code}
                }}}
    end;
decode_error(_, ID, Code, _, _) when not is_binary(Code) ->
    {error,
        {error, #callerror{
            code = 'RpcFrameworkError',
            id = ID,
            description = <<"Invalid error code">>,
            data = #{<<"errorCode">> => Code}
        }}};
decode_error(_, ID, _, Description, _) when not is_binary(Description) ->
    {error,
        {error, #callerror{
            code = 'RpcFrameworkError',
            id = ID,
            description = <<"Invalid error description">>,
            data = #{<<"errorDescription">> => Description}
        }}};
decode_error(_, ID, _, _, Data) ->
    {error,
        {error, #callerror{
            code = 'RpcFrameworkError',
            id = ID,
            description = <<"Invalid error data">>,
            data = #{<<"errorData">> => Data}
        }}}.

decode_request(Type, Version, ID, Action, Payload) when is_binary(Action), is_map(Payload) ->
    case ocpp_message:decode(Version, Action, request, Payload) of
        {ok, Message} when Type =:= call ->
            {ok, {call, #call{id = ID, action = Action, payload = Message}}};
        {ok, Message} when Type =:= send ->
            {ok, {send, #send{id = ID, action = Action, payload = Message}}};
        {error, {badvalue, {extra_key, Key}}} ->
            {error,
                {callerror, #callerror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"unallowed property found in payload">>,
                    data = #{<<"invalidKey">> => Key}
                }}};
        {error, {badvalue, _Reason}} ->
            {error,
                {callerror, #callerror{
                    code = 'PropertyConstraintViolation',
                    id = ID,
                    description = <<"invalid value in payload">>
                }}};
        {error, {badtype, _Reason}} ->
            {error,
                {callerror, #callerror{
                    code = 'TypeConstraintViolation',
                    id = ID,
                    description = <<"invalid type in payload">>
                }}};
        {error, {missing_keys, Missing}} ->
            {error,
                {callerror, #callerror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"payload missing required properties">>,
                    data = #{<<"missingProperties">> => Missing}
                }}}
    end;
decode_request(_, _, ID, Action, _) when not is_binary(Action) ->
    {error,
        {callerror, #callerror{
            code = 'RpcFrameworkError', id = ID, description = <<"Invalid action">>
        }}};
decode_request(_, _, ID, _, Payload) when not is_map(Payload) ->
    {error,
        {callerror, #callerror{
            code = 'ProtocolError', id = ID, description = <<"Payload is not an object">>
        }}}.

decode_result(Version, ID, Payload, ExpectedAction) when is_map(Payload) ->
    ErrorTag =
        if
            Version =:= '2.1' -> callresulterror;
            true -> error
        end,
    case ocpp_message:decode(Version, ExpectedAction, response, Payload) of
        {ok, Message} ->
            {ok, {callresult, #callresult{id = ID, payload = Message}}};
        {error, {missing_keys, Missing}} ->
            {error,
                {ErrorTag, #callresulterror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"Missing required properties">>,
                    data = #{<<"missingProperties">> => Missing}
                }}};
        {error, {badvalue, {extra_key, Key}}} ->
            {error,
                {ErrorTag, #callresulterror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"unallowed property found in payload">>,
                    data = #{<<"invalidKey">> => Key}
                }}};
        {error, {badvalue, _Reason}} ->
            {error,
                {ErrorTag, #callresulterror{
                    code = 'PropertyConstraintViolation',
                    id = ID,
                    description = <<"invalid value in payload">>
                }}};
        {error, {badtype, _}} ->
            {error,
                {ErrorTag, #callresulterror{
                    code = 'TypeConstraintViolation',
                    id = ID,
                    description = <<"invalid type in payload">>
                }}}
    end;
decode_result(Version, ID, _, _) ->
    ErrorTag =
        if
            Version =:= '2.1' -> callresulterror;
            true -> error
        end,
    {error,
        {ErrorTag, #callresulterror{
            code = 'ProtocolError', id = ID, description = <<"Payload is not an object">>
        }}}.

error_type(#callerror{}) ->
    callerror;
error_type(#callresulterror{}) ->
    callresulterror.

error_code(#callerror{code = Code}) ->
    Code;
error_code(#callresulterror{code = Code}) ->
    Code.

error_description(#callerror{description = Description}) ->
    Description;
error_description(#callresulterror{description = Description}) ->
    Description.

id(#call{id = ID}) ->
    ID;
id(#send{id = ID}) ->
    ID;
id(#callresult{id = ID}) ->
    ID;
id(#callerror{id = ID}) ->
    ID;
id(#callresulterror{id = ID}) ->
    ID.

payload(#call{payload = Payload}) ->
    Payload;
payload(#callresult{payload = Payload}) ->
    Payload;
payload(#send{payload = Payload}) ->
    Payload.
