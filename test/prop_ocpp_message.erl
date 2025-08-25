-module(prop_ocpp_message).
-moduledoc """
Useful proper generators for all ocpp messages.
""".

-include_lib("proper/include/proper.hrl").

-export([identifier_string/1, password_string/1, ocpp_string/0, ocpp_string/1, date_time/0]).
-export([
    ocpp_integer/0, ocpp_integer/2, ocpp_non_neg_integer/0, ocpp_pos_integer/0, ocpp_neg_integer/0
]).
-export([x509_client_cert_pem/0]).
-export([emaid/0, iso14443uid/0, iso15693uid/0, mac_address/0]).
-export([imsi/0, iccid/0]).
-export([language/0]).

identifier_string(MaxLen) ->
    ?LET(
        Len,
        integer(0, MaxLen),
        ?LET(
            IDStr,
            vector(
                Len,
                oneof(
                    "abcdefghijklmnopqrstuvwxyz"
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    "0123456789"
                    "*-_-:+|@."
                )
            ),
            list_to_binary(IDStr)
        )
    ).

password_string(MaxLen) ->
    ?LET(
        Len,
        integer(0, MaxLen),
        ?LET(
            PwdStr,
            vector(
                Len,
                oneof(
                    "abcdefghijklmnopqrstuvwxyz"
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    "0123456789"
                    "*-_=:+|@"
                )
            ),
            list_to_binary(PwdStr)
        )
    ).

second() ->
    frequency([
        {1, float(0.0, 59.999)},
        {9, integer(0, 59)}
    ]).

offset() ->
    ?LET(
        Offset,
        frequency([{1, "Z"}, {1, "z"}, {2, {integer(-23, 23), integer(0, 59)}}]),
        case Offset of
            Offset when is_list(Offset) -> Offset;
            {H, M} when H >= 0 ->
                lists:flatten(io_lib:format("+~2..0b:~2..0b", [H, M]));
            {H, M} when H < 0 ->
                lists:flatten(io_lib:format("-~2..0b:~2..0b", [abs(H), M]))
        end
    ).

date_time() ->
    ?LET(
        {Year, Month, Day},
        ?SUCHTHAT(
            Date,
            {integer(1970, 2550), integer(1, 12), integer(1, 31)},
            calendar:valid_date(Date)
        ),
        ?LET(
            {Hour, Minute, Second, Offset},
            {integer(0, 23), integer(0, 59), second(), offset()},
            ?LET(
                T,
                oneof("Tt"),
                begin
                    PartialTime =
                        io_lib:format(
                            "~4..0b-~2..0b-~2..0b~c~2..0b:~2..0b:",
                            [Year, Month, Day, T, Hour, Minute]
                        ),
                    FullDateTime =
                        PartialTime ++
                            if
                                is_integer(Second) -> io_lib:format("~2..0b", [Second]);
                                is_float(Second) -> io_lib:format("~6.3.0f", [Second])
                            end ++
                            Offset,
                    list_to_binary(lists:flatten(FullDateTime))
                end
            )
        )
    ).

ocpp_integer() ->
    %% A 32 bit integer.
    proper_types:integer(-16#7fffffff, 16#7ffffff).

ocpp_integer(Min, Max) ->
    proper_types:integer(max(Min, -16#7fffffff), min(Max, 16#7fffffff)).

ocpp_pos_integer() ->
    proper_types:integer(1, 16#7fffffff).

ocpp_non_neg_integer() ->
    proper_types:integer(0, 16#7fffffff).

ocpp_neg_integer() ->
    proper_types:integer(-16#7fffffff, -1).

ocpp_string() ->
    ?LET(Str, proper_unicode:utf8_string(), unicode:characters_to_binary(Str)).

ocpp_string(MaxLen) ->
    ?LET(Str, proper_unicode:utf8_string(MaxLen), unicode:characters_to_binary(Str)).

%% This is not a generator, it is just a helper function
x509_client_cert_pem() ->
    TestData = public_key:pkix_test_data(
        #{root => [{key, {namedCurve, ed25519}}], peer => []}
    ),
    DERCert = proplists:get_value(cert, TestData),
    public_key:pem_encode([{'Certificate', DERCert, not_encrypted}]).

iso_assigned_country_code() ->
    oneof([
        "AD",
        "AE",
        "AF",
        "AG",
        "AI",
        "AL",
        "AM",
        "AO",
        "AQ",
        "AR",
        "AS",
        "AT",
        "AU",
        "AW",
        "AX",
        "AZ",
        "BA",
        "BB",
        "BD",
        "BE",
        "BF",
        "BG",
        "BH",
        "BI",
        "BJ",
        "BL",
        "BM",
        "BN",
        "BO",
        "BQ",
        "BR",
        "BS",
        "BT",
        "BV",
        "BW",
        "BY",
        "BZ",
        "CA",
        "CC",
        "CD",
        "CF",
        "CG",
        "CH",
        "CI",
        "CK",
        "CL",
        "CM",
        "CN",
        "CO",
        "CR",
        "CU",
        "CV",
        "CW",
        "CX",
        "CY",
        "CZ",
        "DE",
        "DJ",
        "DK",
        "DM",
        "DO",
        "DZ",
        "EC",
        "EE",
        "EG",
        "EH",
        "ER",
        "ES",
        "ET",
        "FI",
        "FJ",
        "FK",
        "FM",
        "FO",
        "FR",
        "GA",
        "GB",
        "GD",
        "GE",
        "GF",
        "GG",
        "GH",
        "GI",
        "GL",
        "GM",
        "GN",
        "GP",
        "GQ",
        "GR",
        "GS",
        "GT",
        "GU",
        "GW",
        "GY",
        "HK",
        "HM",
        "HN",
        "HR",
        "HT",
        "HU",
        "ID",
        "IE",
        "IL",
        "IM",
        "IN",
        "IO",
        "IQ",
        "IR",
        "IS",
        "IT",
        "JE",
        "JM",
        "JO",
        "JP",
        "KE",
        "KG",
        "KH",
        "KI",
        "KM",
        "KN",
        "KP",
        "KR",
        "KW",
        "KY",
        "KZ",
        "LA",
        "LB",
        "LC",
        "LI",
        "LK",
        "LR",
        "LS",
        "LT",
        "LU",
        "LV",
        "LY",
        "MA",
        "MC",
        "MD",
        "ME",
        "MF",
        "MG",
        "MH",
        "MK",
        "ML",
        "MM",
        "MN",
        "MO",
        "MP",
        "MQ",
        "MR",
        "MS",
        "MT",
        "MU",
        "MV",
        "MW",
        "MX",
        "MY",
        "MZ",
        "NA",
        "NC",
        "NE",
        "NF",
        "NG",
        "NI",
        "NL",
        "NO",
        "NP",
        "NR",
        "NU",
        "NZ",
        "OM",
        "PA",
        "PE",
        "PF",
        "PG",
        "PH",
        "PK",
        "PL",
        "PM",
        "PN",
        "PR",
        "PS",
        "PT",
        "PW",
        "PY",
        "QA",
        "RE",
        "RO",
        "RS",
        "RU",
        "RW",
        "SA",
        "SB",
        "SC",
        "SD",
        "SE",
        "SG",
        "SH",
        "SI",
        "SJ",
        "SK",
        "SL",
        "SM",
        "SN",
        "SO",
        "SR",
        "SS",
        "ST",
        "SV",
        "SX",
        "SY",
        "SZ",
        "TC",
        "TD",
        "TF",
        "TG",
        "TH",
        "TJ",
        "TK",
        "TL",
        "TM",
        "TN",
        "TO",
        "TR",
        "TT",
        "TV",
        "TW",
        "TZ",
        "UA",
        "UG",
        "UM",
        "US",
        "UY",
        "UZ",
        "VA",
        "VC",
        "VE",
        "VG",
        "VI",
        "VN",
        "VU",
        "WF",
        "WS",
        "YE",
        "YT",
        "ZA",
        "ZM",
        "ZW"
    ]).

alphanumeric() ->
    oneof(lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9)).

emaid_idro() ->
    [iso_assigned_country_code(), provider_id()].

provider_id() ->
    vector(3, alphanumeric()).

emaid_entity() ->
    oneof([emaid_contract_id(), emaid_chargepoint_id()]).

emaid_contract_id() ->
    ?LET(
        {ID, Check},
        {vector(8, alphanumeric()), vector(1, oneof("0123456789"))},
        oneof([
            {["C" ++ ID], ""},
            {["C" ++ ID, Check], ""},
            {["C" ++ ID], "-"},
            {["C" ++ ID, Check], "-"}
        ])
    ).

emaid_chargepoint_id() ->
    ?LET(
        ID,
        ?LET(Len, integer(1, 30), vector(Len, alphanumeric())),
        oneof([
            {[Type ++ ID], Separator}
         || Type <- ["E", "P", "S"], Separator <- ["", "*"]
        ])
    ).

-doc """

ISO 15118 Electro-Mobility Account ID. This generator does not
generate the full range of valid IDs. Rather, it implements the IDs
specified by an EU [IDACS Consortium white paper][1] on ID format and
syntax requirements. Unfortunately I don't have access to ISO 15118 so
this will be good enough.

[1]: https://evroaming.org/wp-content/uploads/2024/10/20211118-PSA-IDACS-whitepaper-ID-Format-and-syntax-v0.4-clean-version.pdf
""".
emaid() ->
    ?LET(
        {IDRO, {EntityID, Separator}},
        {emaid_idro(), emaid_entity()},
        case lists:flatten(string:join(IDRO ++ EntityID, Separator)) of
            ID when length(ID) > 36 ->
                list_to_binary(lists:flatten([IDRO, EntityID]));
            ID ->
                list_to_binary(ID)
        end
    ).

iso14443uid_single() ->
    ?LET(ID, binary(3), <<16#08, ID/binary>>).

iso14443uid_double() ->
    ?LET({UID0, UIDN}, {integer(16#00, 16#80), binary(6)}, <<UID0, UIDN/binary>>).

iso14443uid() ->
    ?LET(
        {ID, How},
        {oneof([iso14443uid_single(), iso14443uid_double()]), oneof([uppercase, lowercase])},
        binary:encode_hex(ID, How)
    ).

iso15693uid() ->
    ?LET(
        {SN, MFC, How},
        {bitstring(36), binary(1), oneof([uppercase, lowercase])},
        binary:encode_hex(<<16#e0:8, MFC/binary, 0:4, 01:8, SN/bitstring>>, How)
    ).

mac_address() ->
    ?LET({MAC, How}, {binary(6), oneof([uppercase, lowercase])}, binary:encode_hex(MAC, How)).

luhn(ID) ->
    S = lists:sum([
        case (((I - 1) rem 2) + 1) * (N - $0) of
            C when C > 9 -> C - 9;
            C -> C
        end
     || {I, N} <- lists:enumerate(lists:reverse(ID))
    ]),
    Check = (10 - (S rem 10)) rem 10,
    ID ++ [$0 + Check].

-doc """
The ICCID numbers generated here are valid in the sense that they
start with 89 and use Luhn's algorithm to compute the check digit at
the end, but the country codes may not be valid.
""".
iccid() ->
    ?LET(
        {CC, IID},
        {integer(1, 999), integer(1, 9999)},
        ?LET(
            IAINLen,
            integer(1, 17 - length(integer_to_list(CC) ++ integer_to_list(IID))),
            ?LET(
                IAIN,
                vector(IAINLen, oneof("0123456789")),
                list_to_binary(
                    luhn("89" ++ integer_to_list(CC) ++ integer_to_list(IID) ++ IAIN)
                )
            )
        )
    ).

-doc """
These IMSI numbers not necessarily valid. They may include invalic
country codes and MNC lengths that do not comport with the country
code.
""".
imsi() ->
    ?LET(
        {MCC, MNC},
        {
            vector(3, oneof("0123456789")),
            oneof([
                vector(2, oneof("0123456789")),
                vector(3, oneof("0123456789"))
            ])
        },
        ?LET(
            MSIN,
            vector(15 - (length(MNC) + length(MCC)), oneof("0123456789")),
            list_to_binary(MCC ++ MNC ++ MSIN)
        )
    ).

-doc """
An incomplete set of RFC5646 language codes.
""".
language() ->
    oneof([
        <<"af">>,
        <<"af-ZA">>,
        <<"ar">>,
        <<"ar-AE">>,
        <<"ar-BH">>,
        <<"ar-DZ">>,
        <<"ar-EG">>,
        <<"ar-IQ">>,
        <<"ar-JO">>,
        <<"ar-KW">>,
        <<"ar-LB">>,
        <<"ar-LY">>,
        <<"ar-MA">>,
        <<"ar-OM">>,
        <<"ar-QA">>,
        <<"ar-SA">>,
        <<"ar-SY">>,
        <<"ar-TN">>,
        <<"ar-YE">>,
        <<"az">>,
        <<"az-AZ">>,
        %% <<"az-Cyrl-AZ">>, XXX OCPP limits language Identifiers to 8 characters
        <<"be">>,
        <<"be-BY">>,
        <<"bg">>,
        <<"bg-BG">>,
        <<"bs-BA">>,
        <<"ca">>,
        <<"ca-ES">>,
        <<"cs">>,
        <<"cs-CZ">>,
        <<"cy">>,
        <<"cy-GB">>,
        <<"da">>,
        <<"da-DK">>,
        <<"de">>,
        <<"de-AT">>,
        <<"de-CH">>,
        <<"de-DE">>,
        <<"de-LI">>,
        <<"de-LU">>,
        <<"dv">>,
        <<"dv-MV">>,
        <<"el">>,
        <<"el-GR">>,
        <<"en">>,
        <<"en-AU">>,
        <<"en-BZ">>,
        <<"en-CA">>,
        <<"en-CB">>,
        <<"en-GB">>,
        <<"en-IE">>,
        <<"en-JM">>,
        <<"en-NZ">>,
        <<"en-PH">>,
        <<"en-TT">>,
        <<"en-US">>,
        <<"en-ZA">>,
        <<"en-ZW">>,
        <<"eo">>,
        <<"es">>,
        <<"es-AR">>,
        <<"es-BO">>,
        <<"es-CL">>,
        <<"es-CO">>,
        <<"es-CR">>,
        <<"es-DO">>,
        <<"es-EC">>,
        <<"es-ES">>,
        <<"es-GT">>,
        <<"es-HN">>,
        <<"es-MX">>,
        <<"es-NI">>,
        <<"es-PA">>,
        <<"es-PE">>,
        <<"es-PR">>,
        <<"es-PY">>,
        <<"es-SV">>,
        <<"es-UY">>,
        <<"es-VE">>,
        <<"et">>,
        <<"et-EE">>,
        <<"eu">>,
        <<"eu-ES">>,
        <<"fa">>,
        <<"fa-IR">>,
        <<"fi">>,
        <<"fi-FI">>,
        <<"fo">>,
        <<"fo-FO">>,
        <<"fr">>,
        <<"fr-BE">>,
        <<"fr-CA">>,
        <<"fr-CH">>,
        <<"fr-FR">>,
        <<"fr-LU">>,
        <<"fr-MC">>,
        <<"gl">>,
        <<"gl-ES">>,
        <<"gu">>,
        <<"gu-IN">>,
        <<"he">>,
        <<"he-IL">>,
        <<"hi">>,
        <<"hi-IN">>,
        <<"hr">>,
        <<"hr-BA">>,
        <<"hr-HR">>,
        <<"hu">>,
        <<"hu-HU">>,
        <<"hy">>,
        <<"hy-AM">>,
        <<"id">>,
        <<"id-ID">>,
        <<"is">>,
        <<"is-IS">>,
        <<"it">>,
        <<"it-CH">>,
        <<"it-IT">>,
        <<"ja">>,
        <<"ja-JP">>,
        <<"ka">>,
        <<"ka-GE">>,
        <<"kk">>,
        <<"kk-KZ">>,
        <<"kn">>,
        <<"kn-IN">>,
        <<"ko">>,
        <<"ko-KR">>,
        <<"kok">>,
        <<"kok-IN">>,
        <<"ky">>,
        <<"ky-KG">>,
        <<"lt">>,
        <<"lt-LT">>,
        <<"lv">>,
        <<"lv-LV">>,
        <<"mi">>,
        <<"mi-NZ">>,
        <<"mk">>,
        <<"mk-MK">>,
        <<"mn">>,
        <<"mn-MN">>,
        <<"mr">>,
        <<"mr-IN">>,
        <<"ms">>,
        <<"ms-BN">>,
        <<"ms-MY">>,
        <<"mt">>,
        <<"mt-MT">>,
        <<"nb">>,
        <<"nb-NO">>,
        <<"nl">>,
        <<"nl-BE">>,
        <<"nl-NL">>,
        <<"nn-NO">>,
        <<"ns">>,
        <<"ns-ZA">>,
        <<"pa">>,
        <<"pa-IN">>,
        <<"pl">>,
        <<"pl-PL">>,
        <<"ps">>,
        <<"ps-AR">>,
        <<"pt">>,
        <<"pt-BR">>,
        <<"pt-PT">>,
        <<"qu">>,
        <<"qu-BO">>,
        <<"qu-EC">>,
        <<"qu-PE">>,
        <<"ro">>,
        <<"ro-RO">>,
        <<"ru">>,
        <<"ru-RU">>,
        <<"sa">>,
        <<"sa-IN">>,
        <<"se">>,
        <<"se-FI">>,
        <<"se-NO">>,
        <<"se-SE">>,
        <<"sk">>,
        <<"sk-SK">>,
        <<"sl">>,
        <<"sl-SI">>,
        <<"sq">>,
        <<"sq-AL">>,
        <<"sr-BA">>,
        %% <<"sr-Cyrl-BA">>,
        <<"sr-SP">>,
        %% <<"sr-Cyrl-SP">>,
        <<"sv">>,
        <<"sv-FI">>,
        <<"sv-SE">>,
        <<"sw">>,
        <<"sw-KE">>,
        <<"syr">>,
        <<"syr-SY">>,
        <<"ta">>,
        <<"ta-IN">>,
        <<"te">>,
        <<"te-IN">>,
        <<"th">>,
        <<"th-TH">>,
        <<"tl">>,
        <<"tl-PH">>,
        <<"tn">>,
        <<"tn-ZA">>,
        <<"tr">>,
        <<"tr-TR">>,
        <<"tt">>,
        <<"tt-RU">>,
        <<"ts">>,
        <<"uk">>,
        <<"uk-UA">>,
        <<"ur">>,
        <<"ur-PK">>,
        <<"uz">>,
        <<"uz-UZ">>,
        %% <<"uz-Cyrl-U">>,
        <<"vi">>,
        <<"vi-VN">>,
        <<"xh">>,
        <<"xh-ZA">>,
        <<"zh">>,
        <<"zh-CN">>,
        <<"zh-HK">>,
        <<"zh-MO">>,
        <<"zh-SG">>,
        <<"zh-TW">>,
        <<"zu">>,
        <<"zu-ZA">>
    ]).
