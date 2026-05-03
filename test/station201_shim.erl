-module(station201_shim).

-compile(export_all).

connect_unsupported(StationID, Versions) ->
    ocpp_station:connect(StationID, Versions).
connect_supported(StationID, Versions) ->
    ocpp_station:connect(StationID, Versions).
connect_already_connected(StationID, Versions) ->
    ocpp_station:connect(StationID, Versions).

station_rpccall_not_booted(StationID, Message) ->
    RPCCall = ocpp_rpc:call(Message, messageid()),
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCCall)).

csms_rpccall_not_booted(StationID, Message) ->
    ocpp_station:call(StationID, messageid(), Message).

boot(StationID, RPCCall) ->
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCCall)).

accept_boot(StationID, RPCCall, Reply) ->
    valid_reply(StationID, ocpp_rpc:id(RPCCall), Reply).

pending_boot(StationID, RPCCall, Reply) ->
    valid_reply(StationID, ocpp_rpc:id(RPCCall), Reply).

reject_boot(StationID, RPCCall, Reply) ->
    valid_reply(StationID, ocpp_rpc:id(RPCCall), Reply).

heartbeat(StationID, RPCCall) ->
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCCall)).

valid_reply(StationID, MessageID, Message) ->
    case ocpp_station:reply(StationID, MessageID, Message) of
        ok ->
            recv(ocpp_message:action(Message));
        {error, _} = Error ->
            Error
    end.

recv(Action) ->
    receive
        {ocpp, {rpcsend, ReplyBin}} ->
            ocpp_rpc:decode('2.0.1', ReplyBin, [{expected, Action}])
    after 50 -> timeout
    end.

messageid() ->
    integer_to_binary(erlang:unique_integer([positive]), 36).
