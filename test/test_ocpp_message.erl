-module(test_ocpp_message).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

decode_201_test_() ->
    {foreach, fun() -> ocpp_message:unload('2.0.1') end, [
        {"HearbeatRequest can be decoded",
            ?_test(
                begin
                    ?assertMatch({ok, _}, ocpp_message:decode('2.0.1', ~"HeartbeatRequest", #{})),
                    {ok, Msg} = ocpp_message:decode('2.0.1', ~"HeartbeatRequest", #{}),
                    ?assertEqual(~"Heartbeat", ocpp_message:action(Msg))
                end
            )},
        {"HeartbeatResponse can be decoded",
            ?_test(
                begin
                    Now = ~"2026-04-24T12:12:12Z",
                    HBR = #{~"currentTime" => Now},
                    Res = ocpp_message:decode('2.0.1', ~"HeartbeatResponse", HBR),
                    ?assertMatch({ok, _}, Res),
                    {ok, Msg} = Res,
                    ?assertEqual(datetime:from_rfc3339(Now), ocpp_message:get('currentTime', Msg))
                end
            )}
    ]}.

unknown_message_test_() ->
    [
        {"Attempt to decode a message from an unknown version fails",
            ?_assertError(
                {unsupported_version, '1.1'}, ocpp_message:decode('1.1', ~"HeartbeatRequest", #{})
            )},
        {"Attempt to decode a message that does not exist in a known version",
            ?_assertError(
                {unknown_message, ~"NotifyPeriodicEventStream"},
                ocpp_message:decode(
                    '2.0.1',
                    ~"NotifyPeriodicEventStream",
                    #{
                        ~"id" => 0,
                        ~"pending" => 0,
                        ~"basetime" => ~"2026-04-25T12:11:10Z",
                        ~"data" => [#{~"t" => 0.1, ~"v" => ~""}]
                    }
                )
            )}
    ].

all_supported_versions_test_() ->
    [
        {"load version 1.6",
            ?_assertMatch({ok, _}, ocpp_message:decode('1.6', ~"HeartbeatRequest", #{}))},
        {"load version 2.0.1",
            ?_assertMatch({ok, _}, ocpp_message:decode('2.0.1', ~"HeartbeatRequest", #{}))},
        {"load version 2.1",
            ?_assertMatch({ok, _}, ocpp_message:decode('2.1', ~"HeartbeatRequest", #{}))}
    ].

construct_message_test_() ->
    {"construct a message from a map that is not a json:decode_value()",
        ?_test(
            begin
                DT = {{2026, 06, 01}, {22, 11, 00}},
                Res = ocpp_message:new('2.0.1', ~"HeartbeatResponse", #{currentTime => DT}),
                ?assertMatch({ok, _}, Res),
                {ok, Msg} = Res,
                ?assertEqual(datetime:from_local_time(DT), ocpp_message:get(currentTime, Msg))
            end
        )}.

override_test_() ->
    [
        {"EVSEType.id constraint is overriden to require values > 0",
            ?_assertEqual(
                {error,
                    {value_error, #{key => [evse, id], value => 0, spec => {integer, #{min => 1}}}}},
                ocpp_message:new(
                    '2.0.1',
                    ~"ChangeAvailabilityRequest",
                    #{
                        operationalStatus => 'Operative',
                        evse => #{id => 0}
                    }
                )
            )},
        {"Non-overridden properties still validate correctly", [
            ?_assertMatch(
                {ok, _},
                ocpp_message:new('2.0.1', ~"ChangeAvailabilityRequest", #{
                    operationalStatus => 'Operative',
                    evse => #{id => 1, connectorId => 0}
                })
            ),
            ?_assertEqual(
                {error,
                    {type_error, #{
                        key => [evse, connectorId], value => 0.0, spec => {integer, #{}}
                    }}},
                ocpp_message:new('2.0.1', ~"ChangeAvailabilityRequest", #{
                    operationalStatus => 'Operative', evse => #{id => 1, connectorId => 0.0}
                })
            )
        ]}
    ].
