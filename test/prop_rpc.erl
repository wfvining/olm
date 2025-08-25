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
