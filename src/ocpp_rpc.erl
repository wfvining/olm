-module(ocpp_rpc).

-export([decode/3, encode/1]).
-export([call/2, callresult/2, callerror/3]).
-export([id/1, error_type/1, error_code/1, error_description/1, error_details/1]).
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

-record(rpccallerror, {
    id = <<"-1">> :: binary(),
    code :: error_code(),
    description = <<"">> :: binary(),
    data = #{} :: #{binary() => json:decode_value()}
}).

-record(rpccallresulterror, {
    id = <<"-1">> :: binary(),
    code :: error_code(),
    description = <<"">> :: binary(),
    data = #{} :: #{binary() => json:decode_value()}
}).

-record(rpccall, {id :: binary(), action :: binary(), payload :: ocpp_message:message()}).
-record(rpcsend, {id :: binary(), action :: binary(), payload :: ocpp_message:message()}).
-record(rpccallresult, {id :: binary(), payload :: ocpp_message:message()}).

-opaque callerror() :: #rpccallerror{}.
-opaque callresulterror() :: #rpccallresulterror{}.
-opaque call() :: #rpccall{}.
-opaque send() :: #rpcsend{}.
-opaque callresult() :: #rpccallresult{}.

-type rpctype() :: call | send | callresult | callerror | callresulterror.

-doc """
Decode an RPC message. If decoding is succesful a tuple `{ok, RPC}` is
returned where `RPC` is a tuple containing the message type and the
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
    ExpectedAction = proplists:get_value(expected, Options),
    try json:decode(RPCBinary) of
        [2, ID, Action, Payload] ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_request(call, Version, ID, Action, Payload);
                {error, _} ->
                    {error,
                        {callerror, #rpccallerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [2 | Rest] ->
            ID = maybe_message_id(Rest),
            {error,
                {callerror, #rpccallerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALL">>
                }}};
        [3, ID, Payload] ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_result(
                        Version, ID, Payload, ExpectedAction
                    );
                {error, _} when Version =:= '2.1' ->
                    {error,
                        {callresulterror, #rpccallresulterror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}};
                {error, _} ->
                    {error,
                        {error, #rpccallerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [3 | Rest] when Version =:= '2.1' ->
            ID = maybe_message_id(Rest),
            {error,
                {callresulterror, #rpccallresulterror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLRESULT">>
                }}};
        [3 | Rest] ->
            ID = maybe_message_id(Rest),
            {error,
                {error, #rpccallerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLRESULT">>
                }}};
        [4, ID, ErrorCode, Description, Data] ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_error(rpccallerror, ID, ErrorCode, Description, Data);
                {error, _} ->
                    {error,
                        {error, #rpccallerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [4 | Rest] ->
            ID = maybe_message_id(Rest),
            {error,
                {error, #rpccallerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLERROR">>
                }}};
        [5, ID, ErrorCode, Description, Data] when Version =:= '2.1' ->
            case validate_message_id(ID) of
                {ok, ID} ->
                    decode_error(rpccallresulterror, ID, ErrorCode, Description, Data);
                {error, _} ->
                    {error,
                        {error, #rpccallerror{
                            code = 'RpcFrameworkError', description = <<"Invalid message ID">>
                        }}}
            end;
        [5 | Rest] when Version =:= '2.1' ->
            ID = maybe_message_id(Rest),
            {error,
                {error, #rpccallerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid CALLRESULTERROR">>
                }}};
        [5 | Rest] ->
            Vsn = atom_to_binary(Version),
            ID = maybe_message_id(Rest),
            {error,
                {callerror, #rpccallerror{
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
                        {error, #rpccallerror{
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
                {error, #rpccallerror{
                    code = 'RpcFrameworkError', id = ID, description = <<"Invalid SEND">>
                }}};
        [6 | Rest] ->
            ID = maybe_message_id(Rest),
            Vsn = atom_to_binary(Version),
            {error,
                {callerror, #rpccallerror{
                    code = 'MessageTypeNotSupported',
                    id = ID,
                    description = <<"SEND not supported by OCPP version ", Vsn/binary>>
                }}};
        [TypeID | Rest] when is_integer(TypeID) ->
            ID = maybe_message_id(Rest),
            TID = integer_to_binary(TypeID),
            Vsn = atom_to_binary(Version),
            {error,
                {callerror, #rpccallerror{
                    code = 'MessageTypeNotSupported',
                    id = ID,
                    description =
                        <<"Message type ", TID/binary, " not supported by OCPP version ",
                            Vsn/binary>>
                }}};
        _ ->
            {error,
                {error, #rpccallerror{
                    code = 'RpcFrameworkError',
                    description = <<"Invalid RPC wrapper">>
                }}}
    catch
        error:_ ->
            {error,
                {error, #rpccallerror{
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
            {ok, {error_tag(Type), {Type, ID, ErrorCode, Description, Data}}}
    catch
        error:badarg ->
            {error,
                {error, #rpccallerror{
                    code = 'RpcFrameworkError',
                    id = ID,
                    description = <<"Unknown error code">>,
                    data = #{<<"errorCode">> => Code}
                }}}
    end;
decode_error(_, ID, Code, _, _) when not is_binary(Code) ->
    {error,
        {error, #rpccallerror{
            code = 'RpcFrameworkError',
            id = ID,
            description = <<"Invalid error code">>,
            data = #{<<"errorCode">> => Code}
        }}};
decode_error(_, ID, _, Description, _) when not is_binary(Description) ->
    {error,
        {error, #rpccallerror{
            code = 'RpcFrameworkError',
            id = ID,
            description = <<"Invalid error description">>,
            data = #{<<"errorDescription">> => Description}
        }}};
decode_error(_, ID, _, _, Data) ->
    {error,
        {error, #rpccallerror{
            code = 'RpcFrameworkError',
            id = ID,
            description = <<"Invalid error data">>,
            data = #{<<"errorData">> => Data}
        }}}.

error_tag(rpccallerror) ->
    callerror;
error_tag(rpccallresutlerror) ->
    callresulterror.

decode_request(Type, Version, ID, Action, Payload) when is_binary(Action), is_map(Payload) ->
    case ocpp_message:decode(Version, <<Action/binary, "Request">>, Payload) of
        {ok, Message} when Type =:= call ->
            {ok, {call, #rpccall{id = ID, action = Action, payload = Message}}};
        {ok, Message} when Type =:= send ->
            {ok, {send, #rpcsend{id = ID, action = Action, payload = Message}}};
        {error, {badvalue, {extra_key, Key}}} ->
            {error,
                {callerror, #rpccallerror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"unallowed property found in payload">>,
                    data = #{<<"invalidKey">> => Key}
                }}};
        {error, {badvalue, _Reason}} ->
            {error,
                {callerror, #rpccallerror{
                    code = 'PropertyConstraintViolation',
                    id = ID,
                    description = <<"invalid value in payload">>
                }}};
        {error, {badtype, _Reason}} ->
            {error,
                {callerror, #rpccallerror{
                    code = 'TypeConstraintViolation',
                    id = ID,
                    description = <<"invalid type in payload">>
                }}};
        {error, {missing_keys, Missing}} ->
            {error,
                {callerror, #rpccallerror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"payload missing required properties">>,
                    data = #{<<"missingProperties">> => Missing}
                }}}
    end;
decode_request(_, _, ID, Action, _) when not is_binary(Action) ->
    {error,
        {callerror, #rpccallerror{
            code = 'RpcFrameworkError', id = ID, description = <<"Invalid action">>
        }}};
decode_request(_, _, ID, _, Payload) when not is_map(Payload) ->
    {error,
        {callerror, #rpccallerror{
            code = 'ProtocolError', id = ID, description = <<"Payload is not an object">>
        }}}.

decode_result(Version, ID, _, undefined) ->
    ErrorTag =
        if
            Version =:= '2.1' -> callresulterror;
            true -> error
        end,
    {error,
        {ErrorTag, #rpccallresulterror{
            code = 'InternalError', id = ID, description = <<"Result action unknown">>, data = #{}
        }}};
decode_result(Version, ID, Payload, ExpectedAction) when is_map(Payload) ->
    ErrorTag =
        if
            Version =:= '2.1' -> callresulterror;
            true -> error
        end,
    case ocpp_message:decode(Version, <<ExpectedAction/binary, "Response">>, Payload) of
        {ok, Message} ->
            {ok, {callresult, #rpccallresult{id = ID, payload = Message}}};
        {error, {missing_keys, Missing}} ->
            {error,
                {ErrorTag, #rpccallresulterror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"Missing required properties">>,
                    data = #{<<"missingProperties">> => Missing}
                }}};
        {error, {badvalue, {extra_key, Key}}} ->
            {error,
                {ErrorTag, #rpccallresulterror{
                    code = 'OccurenceConstraintViolation',
                    id = ID,
                    description = <<"unallowed property found in payload">>,
                    data = #{<<"invalidKey">> => Key}
                }}};
        {error, {badvalue, _Reason}} ->
            {error,
                {ErrorTag, #rpccallresulterror{
                    code = 'PropertyConstraintViolation',
                    id = ID,
                    description = <<"invalid value in payload">>
                }}};
        {error, {badtype, _}} ->
            {error,
                {ErrorTag, #rpccallresulterror{
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
        {ErrorTag, #rpccallresulterror{
            code = 'ProtocolError', id = ID, description = <<"Payload is not an object">>
        }}}.

-spec encode(call() | callresult() | send() | callerror() | callresulterror()) -> binary().
encode(#rpccall{id = ID, action = Action, payload = Payload}) ->
    iolist_to_binary(
        json:encode(
            [2, ID, Action, Payload],
            fun encoder/2
        )
    );
encode(#rpccallresult{id = ID, payload = Payload}) ->
    iolist_to_binary(json:encode([3, ID, Payload], fun encoder/2));
encode(#rpccallerror{id = ID, code = Code, description = Description, data = Data}) ->
    iolist_to_binary(json:encode([4, ID, Code, Description, Data])).

encoder(Term, _Encoder) when is_tuple(Term) ->
    ocpp_message:encode(Term);
encoder(Term, Encoder) ->
    json:encode_value(Term, Encoder).

-doc """
Construct a CALL RPC message.
""".
-spec call(Message :: ocpp_message:message(), ID :: binary()) -> call().
call(Message, ID) ->
    #rpccall{payload = Message, id = ID, action = ocpp_message:action(Message)}.

-doc """
Construct a CALLRESULT RPC message.
""".
-spec callresult(Message :: ocpp_message:message(), ID :: binary()) -> callresult().
callresult(Message, ID) ->
    #rpccallresult{id = ID, payload = Message}.

-doc """
Construct a CALLERROR RPC message.
""".
-spec callerror(Code :: error_code(), ID :: binary(), Options :: [Option]) -> callerror() when
    Option :: {description, binary()} | {details, json:decode_value()}.
callerror(Code, ID, Options) ->
    Description = proplists:get_value(description, Options, <<"">>),
    Details = proplists:get_value(details, Options, #{}),
    #rpccallerror{code = Code, id = ID, description = Description, data = Details}.

-spec error_type(callerror() | callresulterror()) -> callerror | callresulterror.
error_type(#rpccallerror{}) ->
    callerror;
error_type(#rpccallresulterror{}) ->
    callresulterror.

-spec error_code(callerror() | callresulterror()) -> error_code().
error_code(#rpccallerror{code = Code}) ->
    Code;
error_code(#rpccallresulterror{code = Code}) ->
    Code.

-spec error_description(callerror() | callresulterror()) -> binary().
error_description(#rpccallerror{description = Description}) ->
    Description;
error_description(#rpccallresulterror{description = Description}) ->
    Description.

-spec error_details(callerror() | callresulterror()) -> map().
error_details(#rpccallerror{data = Data}) ->
    Data;
error_details(#rpccallresulterror{data = Data}) ->
    Data.

-spec id(
    ocpp_rpc:call()
    | ocpp_rpc:callresult()
    | ocpp_rpc:callerror()
    | ocpp_rpc:callresulterror()
    | ocpp_rpc:send()
) -> binary().
id(#rpccall{id = ID}) ->
    ID;
id(#rpcsend{id = ID}) ->
    ID;
id(#rpccallresult{id = ID}) ->
    ID;
id(#rpccallerror{id = ID}) ->
    ID;
id(#rpccallresulterror{id = ID}) ->
    ID.

-spec payload(ocpp_rpc:call() | ocpp_rpc:callresult() | ocpp_rpc:send()) -> ocpp_message:message().
payload(#rpccall{payload = Payload}) ->
    Payload;
payload(#rpccallresult{payload = Payload}) ->
    Payload;
payload(#rpcsend{payload = Payload}) ->
    Payload.
