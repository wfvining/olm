-module(prop_ocpp_message_2_0_1).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

combination(Elements) ->
    ?LET(Comb, comb(Elements), lists:concat(Comb)).

comb([]) -> [];
comb([H | T]) -> ?LET(Choice, oneof([[], [H]]), [Choice | comb(T)]).

id_token() ->
    ?LET(
        {IDToken, Type},
        oneof([
            {prop_ocpp_message:identifier_string(36), <<"Central">>},
            {prop_ocpp_message:emaid(), <<"eMAID">>},
            {prop_ocpp_message:iso14443uid(), <<"ISO14443">>},
            {prop_ocpp_message:iso15693uid(), <<"ISO15693">>},
            {prop_ocpp_message:identifier_string(36), <<"KeyCode">>},
            {prop_ocpp_message:identifier_string(36), <<"Local">>},
            {prop_ocpp_message:mac_address(), <<"MacAddress">>},
            {<<"">>, <<"NoAuthorization">>}
        ]),
        #{<<"idToken">> => IDToken, <<"type">> => Type}
    ).

-doc """
Generate a random AuthorizeRequest.

**NOTE** This does not include iso15118CertificateHashData. It may be
added in the future if needed.
""".
authorize_request() ->
    Cert = prop_ocpp_message:x509_client_cert_pem(),
    ?LET(
        {Token, Opts},
        {id_token(), frequency([{4, []}, {1, [{<<"certificate">>, Cert}]}])},
        erlang:iolist_to_binary(json:encode(maps:from_list([{<<"idToken">>, Token} | Opts])))
    ).

authorization_status() ->
    oneof([
        <<"Accepted">>,
        <<"Blocked">>,
        <<"ConcurrentTx">>,
        <<"Expired">>,
        <<"Invalid">>,
        <<"NoCredit">>,
        <<"NotAllowedTypeEVSE">>,
        <<"NotAtThisLocation">>,
        <<"NotAtThisTime">>,
        <<"Unknown">>
    ]).

message_format() ->
    oneof([
        <<"ASCII">>,
        <<"HTML">>,
        <<"URI">>,
        <<"UTF8">>
    ]).

message_content() ->
    ?LET(
        {Format, Content, Opts},
        {
            message_format(),
            %% XXX (probably) won't match the format param
            utf8(512),
            combination([{<<"language">>, prop_ocpp_message:language()}])
        },
        maps:from_list([{<<"format">>, Format}, {<<"content">>, Content} | Opts])
    ).

id_token_info() ->
    ?LET(
        {Status, Opts},
        {
            authorization_status(),
            combination(
                [
                    {<<"cacheExpiryDateTime">>, prop_ocpp_message:date_time()},
                    {<<"chargingPriority">>, integer(-9, 9)},
                    {<<"language1">>, prop_ocpp_message:language()},
                    {<<"evseId">>, non_empty(list(pos_integer()))},
                    {<<"language2">>, prop_ocpp_message:language()},
                    {<<"groupIdToken">>, id_token()},
                    {<<"personalMessage">>, message_content()}
                ]
            )
        },
        maps:from_list([{<<"status">>, Status} | Opts])
    ).

authorize_certificate_status() ->
    oneof([
        <<"Accepted">>,
        <<"SignatureError">>,
        <<"CertificateExpired">>,
        <<"NoCertificateAvailable">>,
        <<"CertChainError">>,
        <<"ContractCancelled">>
    ]).

authorize_response() ->
    ?LET(
        {TokenInfo, Opts},
        {id_token_info(), combination([{<<"certificateStatus">>, authorize_certificate_status()}])},
        iolist_to_binary(json:encode(maps:from_list([{<<"idTokenInfo">>, TokenInfo} | Opts])))
    ).

boot_reason() ->
    oneof([
        <<"ApplicationReset">>,
        <<"FirmwareUpdate">>,
        <<"LocalReset">>,
        <<"PowerUp">>,
        <<"RemoteReset">>,
        <<"ScheduledReset">>,
        <<"Triggered">>,
        <<"Unknown">>,
        <<"Watchdog">>
    ]).

modem() ->
    ?LET(
        Modem,
        combination([
            {<<"iccid">>, prop_ocpp_message:iccid()},
            {<<"imsi">>, prop_ocpp_message:imsi()}
        ]),
        maps:from_list(Modem)
    ).

charging_station() ->
    ?LET(
        {Model, VendorName},
        {utf8(20), utf8(50)},
        ?LET(
            Opts,
            combination([
                {<<"serialNumber">>, utf8(25)},
                {<<"firmwareVersion">>, utf8(50)},
                {<<"modem">>, modem()}
            ]),
            maps:from_list([
                {<<"model">>, Model},
                {<<"vendorName">>, VendorName}
                | Opts
            ])
        )
    ).

boot_notification_request() ->
    ?LET(
        {Reason, CS},
        {boot_reason(), charging_station()},
        erlang:iolist_to_binary(
            json:encode(#{<<"reason">> => Reason, <<"chargingStation">> => CS})
        )
    ).

registration_status() ->
    oneof([<<"Accepted">>, <<"Pending">>, <<"Rejected">>]).

status_info() ->
    ?LET(
        {ReasonCode, Opts},
        {utf8(20), combination([{<<"additionalInfo">>, utf8(512)}])},
        maps:from_list([{<<"reasonCode">>, ReasonCode} | Opts])
    ).

boot_notification_response() ->
    ?LET(
        {Time, Interval, Status, StatusInfo},
        {
            prop_ocpp_message:date_time(),
            non_neg_integer(),
            registration_status(),
            combination([{<<"statusInfo">>, status_info()}])
        },
        erlang:iolist_to_binary(
            json:encode(
                maps:from_list(
                    [
                        {<<"currentTime">>, Time},
                        {<<"interval">>, Interval},
                        {<<"status">>, Status}
                        | StatusInfo
                    ]
                )
            )
        )
    ).

prop_boot_notification_request_type() ->
    ?FORALL(
        BootNotificationRequest,
        boot_notification_request(),
        begin
            {ok, Msg} = ocpp_message:decode(
                '2.0.1',
                <<"BootNotification">>,
                request,
                json:decode(BootNotificationRequest)
            ),
            'BootNotificationRequest' =:= ocpp_message:type(Msg)
        end
    ).

prop_boot_notification_response_type() ->
    ?FORALL(
        BootNotificationResponse,
        boot_notification_response(),
        begin
            {ok, Msg} = ocpp_message:decode(
                '2.0.1',
                <<"BootNotification">>,
                response,
                json:decode(BootNotificationResponse)
            ),
            'BootNotificationResponse' =:= ocpp_message:type(Msg)
        end
    ).

-define(prop_encode_decode(Gen, MsgType, Direction),
    ?FORALL(
        X,
        Gen,
        begin
            {ok, Msg} = ocpp_message:decode(
                '2.0.1',
                MsgType,
                Direction,
                json:decode(X)
            ),
            Encoded = ocpp_message:encode(Msg),
            {ok, ReDecoded} = ocpp_message:decode(
                '2.0.1', MsgType, Direction, json:decode(Encoded)
            ),
            Msg =:= ReDecoded
        end
    )
).

prop_authorize_request_dedode_encode() ->
    ?prop_encode_decode(authorize_request(), <<"Authorize">>, request).

prop_authorize_response_dedode_encode() ->
    ?prop_encode_decode(authorize_response(), <<"Authorize">>, response).

prop_boot_notification_request_decode_encode() ->
    ?prop_encode_decode(boot_notification_request(), <<"BootNotification">>, request).

prop_boot_notification_response_decode_encode() ->
    ?prop_encode_decode(boot_notification_response(), <<"BootNotification">>, response).
