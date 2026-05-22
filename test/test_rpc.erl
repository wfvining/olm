-module(test_rpc).

-include_lib("eunit/include/eunit.hrl").

rpc_framework_error_message_structure_test_() ->
    [
        {"Missing start/end brackets", fun missing_brackets/0},
        {"Missing comma", fun missing_comma/0},
        {"Unterminated string", fun unterminated_string/0},
        {"Extra comma", fun extra_comma/0},
        {"not a list", fun not_a_list/0}
    ].

missing_brackets() ->
    {error, {error, CallError}} = ocpp_rpc:decode(
        '1.6', <<"2,\"foo\",\"BootNotification\",{}]">>, []
    ),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError)),
    {error, {error, CallError1}} = ocpp_rpc:decode(
        '2.0.1', <<"2,\"foo\",\"BootNotification\,{}">>, []
    ),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError1)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError1)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError1)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError1)),
    {error, {error, CallError2}} = ocpp_rpc:decode(
        '2.1', <<"[2,\"foo\",\"BootNotification\",{}">>, []
    ),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError2)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError2)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError2)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError2)).

missing_comma() ->
    {error, {error, CallError}} = ocpp_rpc:decode(
        '1.6', <<"[2 \"id\", \"BootNotification\", {}]">>, []
    ),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError)).

unterminated_string() ->
    {error, {error, CallError}} = ocpp_rpc:decode('2.0.1', <<"[3,\"id,{}]">>, []),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError)).

extra_comma() ->
    {error, {error, CallError}} = ocpp_rpc:decode(
        '1.6', <<"[2, , \"id\", \"BootNotification\", {}]">>, []
    ),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError)),
    {error, {error, CallError1}} = ocpp_rpc:decode('1.6', <<"[,]">>, []),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError1)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError1)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError1)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError1)),
    {error, {error, CallError2}} = ocpp_rpc:decode('1.6', <<"[,,]">>, []),
    ?assertEqual(callerror, ocpp_rpc:error_type(CallError2)),
    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError2)),
    ?assertEqual(<<"Invalid JSON">>, ocpp_rpc:error_description(CallError2)),
    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError2)).

not_a_list() ->
    [
        begin
            {error, {error, CallError}} = ocpp_rpc:decode('2.0.1', Message, []),
            ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
            ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
            ?assertEqual(<<"Invalid RPC wrapper">>, ocpp_rpc:error_description(CallError)),
            ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError))
        end
     || Message <- [<<"\"string\"">>, <<"2">>, <<"{\"a\":1}">>, <<"null">>, <<"2.2">>, <<"true">>]
    ].

invalid_message_type_test_() ->
    BadTypes = [
        <<"\"string\"">>, <<"{}">>, <<"[1, 2, 3]">>, <<"null">>, <<"true">>, <<"false">>, <<"2.2">>
    ],
    {inparallel, [
        {"Invalid message type " ++ binary_to_list(TypeID), fun() ->
            {error, {error, CallError}} =
                ocpp_rpc:decode(
                    '2.0.1', <<"[", TypeID/binary, ",\"id\",\"BootNotification\",{}]">>, []
                ),
            ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
            ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
            ?assertEqual(<<"Invalid RPC wrapper">>, ocpp_rpc:error_description(CallError)),
            ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError))
        end}
     || TypeID <- BadTypes
    ]}.

message_id_not_string_test_() ->
    BadIDs = [<<"1">>, <<"2.2">>, <<"true">>, <<"null">>, <<"[\"foo\"]">>, <<"{}">>],
    {inparallel, [
        {inparallel, [
            {
                "Invalid CALL message ID \"" ++ binary_to_list(BadID) ++
                    "\" results in a CALLERROR",
                fun() ->
                    {error, {callerror, CallError}} =
                        ocpp_rpc:decode(
                            '2.0.1', <<"[2,", BadID/binary, ",\"BootNotification\",{}]">>, []
                        ),
                    ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
                    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
                    ?assertEqual(<<"Invalid message ID">>, ocpp_rpc:error_description(CallError)),
                    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError))
                end
            }
         || BadID <- BadIDs
        ]},
        {inparallel, [
            {
                "(OCPP 2.1) Invalid CALLRESULT message ID \"" ++
                    binary_to_list(BadID) ++ "\" results in CALLRESULTERROR",
                fun() ->
                    {error, {callresulterror, CallError}} =
                        ocpp_rpc:decode('2.1', <<"[3,", BadID/binary, ",{}]">>, []),
                    ?assertEqual(callresulterror, ocpp_rpc:error_type(CallError)),
                    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
                    ?assertEqual(<<"Invalid message ID">>, ocpp_rpc:error_description(CallError)),
                    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError))
                end
            }
         || BadID <- BadIDs
        ]},
        {inparallel, [
            {
                "(OCPP 2.0.1) Invalid CALLRESULT message ID \"" ++
                    binary_to_list(BadID) ++ "\" results in {error, CALLERROR}.",
                fun() ->
                    {error, {error, CallError}} =
                        ocpp_rpc:decode('2.0.1', <<"[3,", BadID/binary, ",{}]">>, []),
                    ?assertEqual(callresulterror, ocpp_rpc:error_type(CallError)),
                    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
                    ?assertEqual(<<"Invalid message ID">>, ocpp_rpc:error_description(CallError)),
                    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError))
                end
            }
         || BadID <- BadIDs
        ]},
        {inparallel, [
            {
                "(OCPP 2.1) Invalid " ++
                    if
                        MType =:= <<"4">> -> "CALLERROR";
                        true -> "CALLRESULTERROR"
                    end ++
                    " message ID \"" ++ binary_to_list(BadID) ++ "\" results in {error, CALLERROR}",
                fun() ->
                    {error, {error, CallError}} = ocpp_rpc:decode(
                        '2.1',
                        <<"[", MType/binary, ",", BadID/binary,
                            ",\"InternalError\",\"Description\",{}]">>,
                        []
                    ),
                    ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
                    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
                    ?assertEqual(<<"Invalid message ID">>, ocpp_rpc:error_description(CallError)),
                    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError))
                end
            }
         || BadID <- BadIDs, MType <- [<<"4">>, <<"5">>]
        ]},
        {inparallel, [
            {
                "(OCPP 2.1) Invalid SEND message ID \"" ++ binary_to_list(BadID) ++
                    "\" results in {error, CALLERROR}",
                fun() ->
                    {error, {error, CallError}} =
                        ocpp_rpc:decode(
                            '2.1', <<"[6,", BadID/binary, ",\"BootNotification\",{}]">>, []
                        ),
                    ?assertEqual(callerror, ocpp_rpc:error_type(CallError)),
                    ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(CallError)),
                    ?assertEqual(<<"Invalid message ID">>, ocpp_rpc:error_description(CallError)),
                    ?assertEqual(<<"-1">>, ocpp_rpc:id(CallError))
                end
            }
         || BadID <- BadIDs
        ]}
    ]}.

msg(2, ID) ->
    <<"[2,\"", ID/binary, "\",\"Heartbeat\",{}]">>;
msg(3, ID) ->
    <<"[3,\"", ID/binary, "\",{\"currentTime\":\"1985-01-01T23:01:01Z\"}]">>;
msg(4, ID) ->
    <<"[4,\"", ID/binary, "\",\"FormatViolation\",\"error description\",{}]">>;
msg(5, ID) ->
    <<"[5,\"", ID/binary, "\",\"FormatViolation\",\"error description\",{}]">>;
msg(6, ID) ->
    <<"[6,\"", ID/binary,
        "\",\"NotifyPeriodicEventStream\","
        "{\"id\":0,\"pending\":0,\"basetime\":\"2025-08-09T11:21:22Z\","
        "\"data\":[{\"t\":22.2,\"v\":\"value\"}]}]">>.

message_id_too_long_test_() ->
    MsgTypes = [2, 3, 4, 5, 6],
    Versions = ['1.6', '2.0.1', '2.1'],
    {inparallel, [
        {
            "(OCPP " ++ atom_to_list(Version) ++
                ") Message ID too long: " ++ binary_to_list(msg(MsgType, <<"...">>)),
            fun() ->
                {Err, ErrType} =
                    if
                        MsgType =:= 2 ->
                            {callerror, callerror};
                        MsgType =:= 3, Version =:= '2.1' ->
                            {callresulterror, callresulterror};
                        MsgType =:= 3 ->
                            {error, callresulterror};
                        true ->
                            {error, callerror}
                    end,
                {error, {Err, Error}} =
                    ocpp_rpc:decode(
                        Version, msg(MsgType, <<"1234567890abcdefghijklmnopqrstuvwxyz1">>), []
                    ),
                ?assertEqual(ErrType, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid message ID">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(<<"-1">>, ocpp_rpc:id(Error))
            end
        }
     || MsgType <- MsgTypes, Version <- Versions, not ((MsgType > 4) and (Version =/= '2.1'))
    ]}.

%% XXX the missing/too-many fields tests will be invalid once signed
%%     messages are supported. At this time the standard is a bit
%%     unclear to me; however, decoding a signed message can probably
%%     be handled easily because the Action field MUST be suffixed
%%     with "-Signed". The mystery in the standard is an additional
%%     field "{<Extension>}" field after the message ID. It is not
%%     referenced anywhere else in the 2.1 or 2.0.1 part 4 documents
%%     so maybe it is just a "{}" object that serves as a sentinel to
%%     indicate a non-standard message.
%%
%%     It is also unclear how to construct a signed CALLRESULT message
%%     since the unsigned message does not include an <Action> field.
%%     The standard, however, doesn't specigy how to a signed payload
%%     in an RPC message that does not contain an <Action>.

call_missing_fields_test_() ->
    Calls = [
        {<<"[2, \"id\", \"BootNotification\"]">>, <<"id">>},
        {<<"[2, \"id\"]">>, <<"id">>},
        {<<"[2]">>, <<"-1">>}
    ],
    Versions = ['1.6', '2.0.1', '2.1'],
    {inparallel, [
        {
            "(OCPP " ++ atom_to_list(Version) ++ ") " ++ binary_to_list(Call) ++
                " results in CALLERROR",
            fun() ->
                {error, {callerror, Error}} = ocpp_rpc:decode(Version, Call, []),
                ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid CALL">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(ID, ocpp_rpc:id(Error))
            end
        }
     || {Call, ID} <- Calls, Version <- Versions
    ]}.

call_too_many_fields_test_() ->
    {inparallel, [
        {
            "(OCPP " ++ atom_to_list(Version) ++ ") CALL with too many fields results in CALLERROR",
            fun() ->
                {error, {callerror, Error}} =
                    ocpp_rpc:decode(Version, <<"[2, \"id\", \"Heartbeat\", {}, {}]">>, []),
                ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid CALL">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(<<"id">>, ocpp_rpc:id(Error))
            end
        }
     || Version <- ['1.6', '2.0.1', '2.1']
    ]}.

callresult_wrong_number_of_fields_test_() ->
    Results = [
        {<<"[3,\"id\",{},\"NotifyPeriodicEventStream\"]">>, <<"id">>},
        {<<"[3,\"id\"]">>, <<"id">>},
        {<<"[3]">>, <<"-1">>}
    ],
    Versions = [
        {'1.6', error, callresulterror},
        {'2.0.1', error, callresulterror},
        {'2.1', callresulterror, callresulterror}
    ],
    {inparallel, [
        {
            "(OCPP " ++ atom_to_list(Version) ++ ") " ++ binary_to_list(Result) ++
                " results in {" ++ atom_to_list(Err) ++ ", " ++
                string:uppercase(atom_to_list(ErrorType)) ++ "}",
            fun() ->
                {error, {Err, Error}} = ocpp_rpc:decode(Version, Result, []),
                ?assertEqual(ErrorType, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid CALLRESULT">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(ID, ocpp_rpc:id(Error))
            end
        }
     || {Result, ID} <- Results, {Version, Err, ErrorType} <- Versions
    ]}.

callerror_wrong_number_of_fields_test_() ->
    Errs = [
        {<<"[4,\"id\",\"RpcFrameworkError\",\"description\",{},{}]">>, <<"id">>},
        {<<"[4,\"id\",\"RpcFrameworkError\",\"desctiption\"]">>, <<"id">>},
        {<<"[4,\"id\",\"RpcFrameworkError\"]">>, <<"id">>},
        {<<"[4,\"id\"]">>, <<"id">>},
        {<<"[4]">>, <<"-1">>}
    ],
    Versions = ['1.6', '2.0.1', '2.1'],
    {inparallel, [
        {
            "(OCPP " ++ atom_to_list(Version) ++ ") " ++ binary_to_list(Err) ++
                " results in {error, CALLERROR}",
            fun() ->
                {error, {error, Error}} = ocpp_rpc:decode(Version, Err, []),
                ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid CALLERROR">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(ID, ocpp_rpc:id(Error))
            end
        }
     || {Err, ID} <- Errs, Version <- Versions
    ]}.

callresulterror_wrong_number_of_fields_test_() ->
    Errs = [
        {<<"[5,\"id\",\"RpcFrameworkError\",\"description\",{},{}]">>, <<"id">>},
        {<<"[5,\"id\",\"RpcFrameworkError\",\"desctiption\"]">>, <<"id">>},
        {<<"[5,\"id\",\"RpcFrameworkError\"]">>, <<"id">>},
        {<<"[5,\"id\"]">>, <<"id">>},
        {<<"[5]">>, <<"-1">>}
    ],
    {inparallel, [
        {
            "(OCPP 2.1) " ++ binary_to_list(Err) ++
                " results in {error, CALLERROR}",
            fun() ->
                {error, {error, Error}} = ocpp_rpc:decode('2.1', Err, []),
                ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid CALLRESULTERROR">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(ID, ocpp_rpc:id(Error))
            end
        }
     || {Err, ID} <- Errs
    ]}.

send_wrong_number_of_fields_test_() ->
    Sends = [
        {<<"[6,\"id\",\"NotifyPeriodicEventStream\",{},{}]">>, <<"id">>},
        {<<"[6,\"id\",\"NotifyPeriodicEventStream\"]">>, <<"id">>},
        {<<"[6,\"id\"]">>, <<"id">>},
        {<<"[6]">>, <<"-1">>}
    ],
    {inparallel, [
        {
            "(OCPP 2.1) " ++ binary_to_list(Send) ++
                " results in {error, CALLERROR}",
            fun() ->
                {error, {error, Error}} = ocpp_rpc:decode('2.1', Send, []),
                ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid SEND">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(ID, ocpp_rpc:id(Error))
            end
        }
     || {Send, ID} <- Sends
    ]}.

call_invalid_action_test_() ->
    Actions = [<<"[\"abc\"]">>, <<"false">>, <<"{}">>, <<"null">>, <<"1">>, <<"1.2">>],
    Versions = ['1.6', '2.0.1', '2.1'],
    {inparallel, [
        {
            "(OCPP " ++ atom_to_list(Version) ++ ") CALL invalid Action \"" ++
                binary_to_list(Action) ++ "\"",
            fun() ->
                {error, {callerror, Error}} =
                    ocpp_rpc:decode(Version, <<"[2,\"id\",", Action/binary, ",{}]">>, []),
                ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
                ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
                ?assertEqual(<<"Invalid action">>, ocpp_rpc:error_description(Error)),
                ?assertEqual(<<"id">>, ocpp_rpc:id(Error))
            end
        }
     || Action <- Actions, Version <- Versions
    ]}.

send_invalid_action_test_() ->
    Actions = [<<"[\"abc\"]">>, <<"false">>, <<"{}">>, <<"null">>, <<"1">>, <<"1.2">>],
    {inparallel, [
        {"SEND invalid Action " ++ binary_to_list(Action), fun() ->
            {error, {error, Error}} =
                ocpp_rpc:decode('2.1', <<"[6,\"id\",", Action/binary, ",{}]">>, []),
            ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
            ?assertEqual('RpcFrameworkError', ocpp_rpc:error_code(Error)),
            ?assertEqual(<<"Invalid action">>, ocpp_rpc:error_description(Error)),
            ?assertEqual(<<"id">>, ocpp_rpc:id(Error))
        end}
     || Action <- Actions
    ]}.

send_not_supported_test_() ->
    [
        {"OCPP 1.6 does not support SEND", fun() ->
            {error, {callerror, Error}} = ocpp_rpc:decode('1.6', msg(6, <<"id">>), []),
            ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
            ?assertEqual('MessageTypeNotSupported', ocpp_rpc:error_code(Error)),
            ?assertEqual(
                <<"SEND not supported by OCPP version 1.6">>, ocpp_rpc:error_description(Error)
            ),
            ?assertEqual(<<"id">>, ocpp_rpc:id(Error))
        end},
        {"OCPP 2.0.1 does not support SEND", fun() ->
            {error, {callerror, Error}} = ocpp_rpc:decode('2.0.1', msg(6, <<"id">>), []),
            ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
            ?assertEqual('MessageTypeNotSupported', ocpp_rpc:error_code(Error)),
            ?assertEqual(
                <<"SEND not supported by OCPP version 2.0.1">>, ocpp_rpc:error_description(Error)
            ),
            ?assertEqual(<<"id">>, ocpp_rpc:id(Error))
        end}
    ].

callresulterror_not_supported_test_() ->
    [
        {"OCPP 1.6 does not support CALLRESULTERROR", fun() ->
            {error, {callerror, Error}} = ocpp_rpc:decode('1.6', msg(5, <<"id">>), []),
            ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
            ?assertEqual('MessageTypeNotSupported', ocpp_rpc:error_code(Error)),
            ?assertEqual(
                <<"CALLRESULTERROR not supported by OCPP version 1.6">>,
                ocpp_rpc:error_description(Error)
            ),
            ?assertEqual(<<"id">>, ocpp_rpc:id(Error))
        end},
        {"OCPP 2.0.1 does not support CALLRESULTERROR", fun() ->
            {error, {callerror, Error}} = ocpp_rpc:decode('2.0.1', msg(5, <<"id">>), []),
            ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
            ?assertEqual('MessageTypeNotSupported', ocpp_rpc:error_code(Error)),
            ?assertEqual(
                <<"CALLRESULTERROR not supported by OCPP version 2.0.1">>,
                ocpp_rpc:error_description(Error)
            ),
            ?assertEqual(<<"id">>, ocpp_rpc:id(Error))
        end}
    ].

generic_unsupported_message_type_test_() ->
    Versions = ['1.6', '2.0.1', '2.1'],
    Messages = [
        {<<"[100]">>, <<"-1">>},
        {<<"[100,\"id\"]">>, <<"id">>},
        {<<"[100,true]">>, <<"-1">>},
        {<<"[100,\"id\",1,2,3]">>, <<"id">>},
        {<<"[100,true,1,2,3]">>, <<"-1">>}
    ],
    [
        {"OCPP version " ++ atom_to_list(Version) ++ "does not support message type 100", fun() ->
            Vsn = atom_to_binary(Version),
            {error, {callerror, Error}} = ocpp_rpc:decode(Version, Msg, []),
            ?assertEqual(callerror, ocpp_rpc:error_type(Error)),
            ?assertEqual('MessageTypeNotSupported', ocpp_rpc:error_code(Error)),
            ?assertEqual(
                <<"Message type 100 not supported by OCPP version ", Vsn/binary>>,
                ocpp_rpc:error_description(Error)
            ),
            ?assertEqual(ID, ocpp_rpc:id(Error))
        end}
     || Version <- Versions, {Msg, ID} <- Messages
    ].

encode_call_test_() ->
    Versions = ['1.6', '2.0.1', '2.1'],
    [
        {"(" ++ atom_to_list(V) ++ ") CALL can be encoded and decoded", fun() ->
            {ok, Msg} = ocpp_message:new(V, ~"HeartbeatRequest", #{}),
            Call = ocpp_rpc:call(Msg, <<"id">>),
            Encoded = ocpp_rpc:encode(Call),
            ?assertEqual({ok, {call, Call}}, ocpp_rpc:decode(V, Encoded, []))
        end}
     || V <- Versions
    ].

encode_callresult_test_() ->
    Versions = ['1.6', '2.0.1', '2.1'],
    [
        {"(" ++ atom_to_list(V) ++ ") CALLRESULT can be encoded and decoded", fun() ->
            {ok, Msg} = ocpp_message:new(V, ~"HeartbeatResponse", #{
                currentTime => {{2025, 1, 1}, {12, 12, 12}}
            }),
            CallResult = ocpp_rpc:callresult(Msg, <<"id">>),
            Encoded = ocpp_rpc:encode(CallResult),
            ?assertEqual(
                {ok, {callresult, CallResult}},
                ocpp_rpc:decode(V, Encoded, [{expected, <<"Heartbeat">>}])
            )
        end}
     || V <- Versions
    ].

encode_error_messages_test_() ->
    Versions = ['1.6', '2.0.1', '2.1'],
    % shared payload for success-path decode
    CallError = ocpp_rpc:callerror('FormatViolation', ~"id", [
        {description, ~"error description"}
    ]),
    CallResultError = ocpp_rpc:callresulterror('FormatViolation', ~"id", [
        {description, ~"error description"}
    ]),
    EncodedCallError = ocpp_rpc:encode(CallError),
    EncodedCallResultError = ocpp_rpc:encode(CallResultError),
    [
        [
            {"(" ++ atom_to_list(V) ++ ") CALLERROR can be encoded and decoded", fun() ->
                {ok, {callerror, Decoded}} = ocpp_rpc:decode(V, EncodedCallError, []),
                ?assertEqual('FormatViolation', ocpp_rpc:error_code(Decoded)),
                ?assertEqual(~"id", ocpp_rpc:id(Decoded)),
                ?assertEqual(~"error description", ocpp_rpc:error_description(Decoded)),
                ?assertEqual(#{}, ocpp_rpc:error_details(Decoded))
            end}
         || V <- Versions
        ],
        [
            {"(2.1) CALLRESULTERROR can be encoded and decoded", fun() ->
                {ok, {callresulterror, Decoded}} = ocpp_rpc:decode(
                    '2.1', EncodedCallResultError, []
                ),
                ?assertEqual('FormatViolation', ocpp_rpc:error_code(Decoded)),
                ?assertEqual(~"id", ocpp_rpc:id(Decoded)),
                ?assertEqual(~"error description", ocpp_rpc:error_description(Decoded)),
                ?assertEqual(#{}, ocpp_rpc:error_details(Decoded))
            end}
        ],
        [
            {"(" ++ atom_to_list(V) ++ ") CALLRESULTERROR not supported", fun() ->
                {error, {callerror, Error}} = ocpp_rpc:decode(V, EncodedCallResultError, []),
                ?assertEqual('MessageTypeNotSupported', ocpp_rpc:error_code(Error))
            end}
         || V <- Versions -- ['2.1']
        ]
    ].

encode_send_test_() ->
    EventData = #{
        eventId => 0,
        timestamp => ~"2026-08-06T01:02:03Z",
        trigger => 'Alerting',
        actualValue => ~"value",
        eventNotificationType => 'CustomMonitor',
        component => #{name => ~"foo"},
        variable => #{name => ~"bar"}
    },
    {ok, NotifyEvent} = ocpp_message:new(
        '2.1',
        ~"NotifyEventRequest",
        #{
            generatedAt => ~"2026-08-08T00:01:02Z",
            seqNo => 1,
            eventData => [EventData]
        }
    ),
    Send = ocpp_rpc:send(NotifyEvent, ~"id"),
    EncodedSend = ocpp_rpc:encode(Send),
    [
        [
            {"(" ++ atom_to_list(V) ++ ") SEND not supported", fun() ->
                {error, {callerror, Error}} = ocpp_rpc:decode(V, EncodedSend, []),
                ?assertEqual('MessageTypeNotSupported', ocpp_rpc:error_code(Error))
            end}
         || V <- ['1.6', '2.0.1']
        ],
        [
            {"(2.1) SEND can be encoded and decoded", fun() ->
                {ok, {send, Decoded}} = ocpp_rpc:decode('2.1', EncodedSend, []),
                ?assertEqual(~"id", ocpp_rpc:id(Decoded)),
                ?assertEqual(~"NotifyEvent", ocpp_rpc:action(Decoded)),
                ?assertEqual(NotifyEvent, ocpp_rpc:payload(Decoded))
            end}
        ]
    ].

decode_callresult_unknown_action_test_() ->
    [
        {
            "(" ++ atom_to_list(Version) ++
                ") CALLRESULT decode returns {error, {unknow_action, ID}}, when {expected, Action} is not specified",
            ?_test(
                %% ClearCacheResponse
                ?assertEqual(
                    {error, {unknown_action, ~"id"}},
                    ocpp_rpc:decode(Version, ~'[3,"id",{"status":"Accepted"}]', [])
                )
            )
        }
     || Version <- ['1.6', '2.0.1', '2.1']
    ].

decode_action_mismatch_test_() ->
    [
        {
            "(" ++ atom_to_list(Version) ++
                ") CALLRESULT decode with mismatched payload returns OccurenceConstraintViolation",
            ?_test(
                begin
                    BootNotificationResponse =
                        ~'[3,"id",{"status":"Accepted","currentTime":"2030-01-01T00:00:00Z","interval":1}]',
                    DecodeResult = ocpp_rpc:decode(Version, BootNotificationResponse, [
                        {expected, ~"ClearCache"}
                    ]),
                    ?assertMatch({error, {_, _}}, DecodeResult),
                    {error, {Kind, Reason}} = DecodeResult,
                    if
                        Version == '2.1' ->
                            ?assertEqual(callresulterror, Kind);
                        true ->
                            ?assertEqual(error, Kind)
                    end,
                    ?assertEqual(callresulterror, ocpp_rpc:error_type(Reason)),
                    ?assertEqual(~"id", ocpp_rpc:id(Reason)),
                    ?assertEqual('OccurenceConstraintViolation', ocpp_rpc:error_code(Reason)),
                    ?assertEqual(
                        <<"unallowed properties found in payload">>,
                        ocpp_rpc:error_description(Reason)
                    )
                end
            )
        }
     || Version <- ['1.6', '2.0.1', '2.1']
    ].

payload_errors_test_() ->
    ExtraCall = <<
        "[2,\"id\",\"BootNotification\",{\"reason\":\"PowerUp\","
        "\"chargingStation\":{\"model\":\"Model\",\"vendorName\":\"Vendor\"},"
        "\"extraKey\":\"value\"}]"
    >>,
    ExtraResult = <<
        "[3,\"id\",{\"status\":\"Accepted\","
        "\"currentTime\":\"2025-01-01T00:00:00Z\","
        "\"interval\":0,\"extraKey\":\"value\"}]"
    >>,
    BadValCall = <<
        "[2,\"id\",\"BootNotification\",{\"reason\":\"InvalidReason\","
        "\"chargingStation\":{\"model\":\"Model\",\"vendorName\":\"Vendor\"}}]"
    >>,
    BadValResult = <<
        "[3,\"id\",{\"status\":\"InvalidStatus\","
        "\"currentTime\":\"2025-01-01T00:00:00Z\",\"interval\":0}]"
    >>,
    BadTypeCall = <<
        "[2,\"id\",\"BootNotification\",{\"reason\":\"PowerUp\","
        "\"chargingStation\":{\"model\":123,\"vendorName\":\"Vendor\"}}]"
    >>,
    BadTypeResult = <<
        "[3,\"id\",{\"status\":\"Accepted\","
        "\"currentTime\":\"2025-01-01T00:00:00Z\",\"interval\":\"bad\"}]"
    >>,
    ViolationSpecs = [
        {"OccurenceConstraintViolation - extra properties", 'OccurenceConstraintViolation',
            ~"unallowed properties found in payload", ExtraCall, ExtraResult},
        {"OccurenceConstraintViolation - missing properties", 'OccurenceConstraintViolation',
            ~"payload missing required properties", ~'[2,"id","BootNotification",{}]',
            ~'[3,"id",{}]'},
        {"PropertyConstraintViolation - bad value", 'PropertyConstraintViolation',
            ~"invalid value in payload", BadValCall, BadValResult},
        {"TypeConstraintViolation - bad type", 'TypeConstraintViolation',
            ~"invalid type in payload", BadTypeCall, BadTypeResult}
    ],
    ProtocolPayloads = [
        {"array", ~"[1,2,3]"},
        {"integer", ~"1"},
        {"float", ~"1.5"},
        {"string", ~'"hello"'},
        {"boolean", ~"true"},
        {"null", ~"null"}
    ],
    CallResultVersions = ['2.0.1', '2.1'],
    [
        [
            call_violation_test(Desc, Code, Description, Msg)
         || {Desc, Code, Description, Msg, _} <- ViolationSpecs
        ],
        [
            callresult_violation_test(V, Desc, Code, Description, Msg)
         || V <- CallResultVersions, {Desc, Code, Description, _, Msg} <- ViolationSpecs
        ],
        [
            call_protocol_error_test(TypeDesc, Payload)
         || {TypeDesc, Payload} <- ProtocolPayloads
        ],
        [
            callresult_protocol_error_test(V, TypeDesc, Payload)
         || V <- CallResultVersions, {TypeDesc, Payload} <- ProtocolPayloads
        ]
    ].

call_violation_test(Desc, Code, Description, Msg) ->
    {"(CALL) " ++ Desc, fun() ->
        {error, {callerror, Error}} = ocpp_rpc:decode('2.0.1', Msg, []),
        ?assertEqual(Code, ocpp_rpc:error_code(Error)),
        ?assertEqual(Description, ocpp_rpc:error_description(Error)),
        ?assertEqual(~"id", ocpp_rpc:id(Error))
    end}.

callresult_violation_test(Version, Desc, Code, Description, Msg) ->
    Opts = [{expected, ~"BootNotification"}],
    {"(CALLRESULT " ++ atom_to_list(Version) ++ ") " ++ Desc, fun() ->
        {error, {ErrorTag, Error}} =
            ocpp_rpc:decode(Version, Msg, Opts),
        case Version of
            '2.1' -> ?assertEqual(callresulterror, ErrorTag);
            _ -> ?assertEqual(error, ErrorTag)
        end,
        ?assertEqual(callresulterror, ocpp_rpc:error_type(Error)),
        ?assertEqual(Code, ocpp_rpc:error_code(Error)),
        ?assertEqual(Description, ocpp_rpc:error_description(Error)),
        ?assertEqual(~"id", ocpp_rpc:id(Error))
    end}.

call_protocol_error_test(TypeDesc, Payload) ->
    Msg = <<"[2,\"id\",\"BootNotification\",", Payload/binary, "]">>,
    {"(CALL) ProtocolError - payload not a JSON object: " ++ TypeDesc, fun() ->
        {error, {callerror, Error}} =
            ocpp_rpc:decode('2.0.1', Msg, []),
        ?assertEqual('ProtocolError', ocpp_rpc:error_code(Error)),
        ?assertEqual(
            ~"Payload is not an object",
            ocpp_rpc:error_description(Error)
        ),
        ?assertEqual(~"id", ocpp_rpc:id(Error))
    end}.

callresult_protocol_error_test(Version, TypeDesc, Payload) ->
    Msg = <<"[3,\"id\",", Payload/binary, "]">>,
    Opts = [{expected, ~"BootNotification"}],
    {
        "(CALLRESULT " ++ atom_to_list(Version) ++
            ") ProtocolError - payload not a JSON object: " ++ TypeDesc,
        fun() ->
            {error, {ErrorTag, Error}} =
                ocpp_rpc:decode(Version, Msg, Opts),
            case Version of
                '2.1' -> ?assertEqual(callresulterror, ErrorTag);
                _ -> ?assertEqual(error, ErrorTag)
            end,
            ?assertEqual(callresulterror, ocpp_rpc:error_type(Error)),
            ?assertEqual('ProtocolError', ocpp_rpc:error_code(Error)),
            ?assertEqual(
                ~"Payload is not an object",
                ocpp_rpc:error_description(Error)
            ),
            ?assertEqual(~"id", ocpp_rpc:id(Error))
        end
    }.

callresult_id_mismatch_test_() ->
    Versions = ['1.6', '2.0.1', '2.1'],
    ExpectedID = ~"expected",
    [
        {
            "(" ++ atom_to_list(Version) ++
                ") "
                "decode fails when ID des not match expected ID",
            ?_assertEqual(
                {error, {badid, ~"bad"}},
                ocpp_rpc:decode(Version, ~'[3,"bad",{}]', [{id, ExpectedID}])
            )
        }
     || Version <- Versions
    ].
