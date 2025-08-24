-module(prop_datetime).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

datetime() ->
    {
        ?SUCHTHAT(
            Date, {integer(1970, 2570), integer(1, 12), integer(1, 31)}, calendar:valid_date(Date)
        ),
        {integer(0, 23), integer(0, 59), integer(0, 59)}
    }.

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

%% This won't be needed in OTP 28
to_system_time(DateTime) when DateTime >= {{1970, 1, 1}, {0, 0, 0}} ->
    Offset = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    Secs - Offset.

rfc3339() ->
    ?LET(
        {{{Year, Month, Day}, {Hour, Minute, Second}}, Precision, Millis, Sep, Offset},
        {
            datetime(),
            integer(0, 3),
            frequency([{95, integer(1, 999)}, {5, 0}]),
            oneof("Tt"),
            offset()
        },
        begin
            PartialDT = io_lib:format(
                "~4..0b-~2..0b-~2..0b~c~2..0b:~2..0b:",
                [Year, Month, Day, Sep, Hour, Minute]
            ),
            Seconds =
                if
                    Precision > 0 ->
                        io_lib:format("~*.*.0f", [
                            3 + Precision, Precision, Second + (Millis / 1000.0)
                        ]);
                    Precision =:= 0 ->
                        io_lib:format("~2..0b", [Second])
                end,
            iolist_to_binary([PartialDT, Seconds, Offset])
        end
    ).

prop_decode_encode() ->
    ?FORALL(
        RFC3339DateTime,
        rfc3339(),
        begin
            DateTime = datetime:from_rfc3339(RFC3339DateTime),
            STMillis = calendar:rfc3339_to_system_time(binary_to_list(RFC3339DateTime), [
                {unit, millisecond}
            ]),
            RFC3339Out = datetime:to_rfc3339(DateTime),
            STMillisOut = calendar:rfc3339_to_system_time(
                unicode:characters_to_list(RFC3339Out), [{unit, millisecond}]
            ),
            (STMillis =:= STMillisOut) and is_binary(RFC3339Out)
        end
    ).

prop_from_local_time() ->
    ?FORALL(
        DateTime,
        datetime(),
        begin
            DT = datetime:from_local_time(DateTime),
            %% UTC timestamp
            TimeStamp = unicode:characters_to_list(datetime:to_rfc3339(DT)),
            %% DateTime to local calendar:datetime()
            DTOut = calendar:rfc3339_to_system_time(TimeStamp),
            UTC = calendar:system_time_to_universal_time(DTOut, second),
            calendar:universal_time_to_local_time(UTC) =:= DateTime
        end
    ).
