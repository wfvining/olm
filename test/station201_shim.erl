-module(station201_shim).

-export([
    connect_unsupported/2,
    connect_supported/2,
    connect_already_connected/2,
    station_call_before_boot/2,
    station_call_security_error/2,
    station_call_boot/2,
    station_call_heartbeat/2,
    csms_call_before_boot/2,
    csms_call_after_boot/3,
    csms_call_set_variables/3,
    csms_reply/3,
    csms_reply_boot_accepted/3,
    csms_reply_boot_pending/3,
    csms_reply_boot_rejected/3,
    station_reply_set_variables/3,
    station_reply_set_variables_expired/3
]).

connect_unsupported(StationID, Versions) ->
    ocpp_station:connect(StationID, Versions).
connect_supported(StationID, Versions) ->
    ocpp_station:connect(StationID, Versions).
connect_already_connected(StationID, Versions) ->
    ocpp_station:connect(StationID, Versions).

station_call_before_boot(StationID, Message) ->
    RPCCall = ocpp_rpc:call(Message, messageid()),
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCCall)).

station_call_security_error(StationID, RPCCall) ->
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCCall)).

station_call_boot(StationID, RPCCall) ->
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCCall)).

station_call_heartbeat(StationID, RPCCall) ->
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCCall)).

csms_call_before_boot(StationID, Message) ->
    ocpp_station:call(StationID, messageid(), Message).

csms_call_after_boot(StationID, Message, MessageID) ->
    ocpp_station:call(StationID, MessageID, Message).

csms_call_set_variables(StationID, MessageID, Request) ->
    ocpp_station:call(StationID, MessageID, Request).

csms_reply(StationID, MessageID, Payload) ->
    ocpp_station:reply(StationID, MessageID, Payload).

csms_reply_boot_accepted(StationID, RPCCall, Reply) ->
    ocpp_station:reply(StationID, ocpp_rpc:id(RPCCall), Reply).

csms_reply_boot_pending(StationID, RPCCall, Reply) ->
    ocpp_station:reply(StationID, ocpp_rpc:id(RPCCall), Reply).

csms_reply_boot_rejected(StationID, RPCCall, Reply) ->
    ocpp_station:reply(StationID, ocpp_rpc:id(RPCCall), Reply).

station_reply_set_variables(StationID, RPCCall, Payload) ->
    RPCReply = ocpp_rpc:callresult(Payload, ocpp_rpc:id(RPCCall)),
    ocpp_station:rpc(StationID, ocpp_rpc:encode(RPCReply)).

station_reply_set_variables_expired(StationID, RPCCall, Payload) ->
    station_reply_set_variables(StationID, RPCCall, Payload).

messageid() ->
    integer_to_binary(erlang:unique_integer([positive]), 36).
