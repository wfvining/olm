-module(ocpp_rpc).

-export([decode/2]).

-type rpc() :: {rpc_type(), binary(), ocpp_message:message()}.

-type rpc_error() :: {rpc_errortype(), binary(), {binary(), binary()}, json:decode_value()}.

-type rpc_errortype() :: callerror | callresulterror.

-type rpc_type() :: call | callresult | send.

-spec decode(Version :: string(), RPCBinary :: binary()) ->
    {ok, rpc() | rpc_error()}.
decode(Version, RPCBinary) ->
    try json:decode(RPCBinary) of
        [3, ID, Payload] ->
            %% CALLRESULT
            MessageType = number_to_message_type(3),
            IDLen = string:length(ID),
            if
                IDLen > 36 ->
                    error({invalid_message_id, ID});
                true ->
                    ok
            end,
            %% XXX This is tricky (and ugly). Beccause the message
            %%     type is not known except by storing it under the
            %%     message ID and the identity of the charging
            %%     station. To punt this problem for later we
            %%     partially decode the message and leave the decoding
            %%     of the payload for later.
            %%
            %%     Later I may be able to store these things in a
            %%     per-station message cache process. This will depend
            %%     on the architecture the unfolds.
            {ok, UndecodedResponse} = ocpp_message:decode_partial(
                Version, type_to_direction(MessageType), Payload
            ),
            {ok, {MessageType, ID, UndecodedResponse}};
        [MsgTypeID, ID, ErrorCode, Description, Details] ->
            %% These messages are always errors of some kind.
            %% TODO not sure if the message ID should be kept if it is invalid
            MessageType = number_to_message_type(MsgTypeID),
            {ok, {MessageType, ID, {ErrorCode, Description}, Details}};
        [MsgTypeID, ID, Action, Payload] ->
            %% CALL or SEND
            MessageType = number_to_message_type(MsgTypeID),
            maybe
                true ?= string:length(ID) =< 36,
                {ok, MessagePayload} ?=
                    ocpp_message:decode(
                        Version,
                        Action,
                        type_to_direction(MessageType),
                        Payload
                    ),
                {ok, {MessageType, ID, MessagePayload}}
            else
                false ->
                    error({invalid_message_id, ID});
                {error, Reason} ->
                    error({invalid_payload, {ID, Reason}})
            end
    catch
        error:Reason ->
            error({decode_failed, Reason})
    end.

type_to_direction(call) ->
    request;
type_to_direction(callresult) ->
    response;
type_to_direction(X) ->
    X.

number_to_message_type(2) ->
    call;
number_to_message_type(3) ->
    callresult;
number_to_message_type(4) ->
    callerror;
number_to_message_type(5) ->
    callresulterror;
number_to_message_type(6) ->
    send;
number_to_message_type(N) ->
    error({invalid_message_type_number, N}).
