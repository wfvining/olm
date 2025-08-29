-module(prop_rpc).

-include_lib("proper/include/proper.hrl").

prop_decode_encode_boot_notification_request() ->
    ?FORALL(
        {Version, BootNotification},
        oneof([{'2.0.1', prop_ocpp_message_2_0_1:boot_notification_request()}]),
        begin
            RPCBin = <<"[2,\"id\",\"BootNotification\",", BootNotification/binary, "]">>,
            {ok, {call, RPC}} = ocpp_rpc:decode(Version, RPCBin, []),
            ReEncoded = ocpp_rpc:encode(RPC),
            {ok, {call, ReDecoded}} = ocpp_rpc:decode(Version, ReEncoded, []),
            (<<"id">> =:= ocpp_rpc:id(RPC)) andalso (ReDecoded =:= RPC)
        end
    ).

prop_call() ->
    ?FORALL(
        {Version, {Action, Message}, ID},
        {'2.0.1',
            oneof([
                {<<"BootNotification">>, prop_ocpp_message_2_0_1:boot_notification_request()},
                {<<"Authorize">>, prop_ocpp_message_2_0_1:authorize_request()}
            ]), utf8(36)},
        begin
            {ok, Msg} = ocpp_message:decode(Version, Action, request, json:decode(Message)),
            Call = ocpp_rpc:call(Msg, ID),
            EncodedCall = ocpp_rpc:encode(Call),
            {ok, {call, DecodedCall}} = ocpp_rpc:decode(Version, EncodedCall, []),
            Call =:= DecodedCall
        end
    ).
