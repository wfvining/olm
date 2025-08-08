-module(test_station).

-include_lib("eunit/include/eunit.hrl").

-define(withStation(StationID, Tests),
    {foreach, fun setup_station/0, fun teardown_station/1, Tests}
).
-define(withConnectedStation(StationID, InState, Tests),
    {foreach, fun() -> setup_connected_station(StationID, InState) end,
        fun teardown_connected_station/1, Tests}
).

setup_station() ->
    setup_station(<<"foo">>, []).

setup_deps() ->
    application:ensure_all_started(gproc).

teardown_deps(_) ->
    application:stop(gproc).

setup_station(StationID, _Options) ->
    ok = meck:new(ocpp_station_manager),
    meck:expect(
        ocpp_station_manager,
        connect,
        fun(_, Versions) -> {ok, lists:max(Versions)} end
    ),
    {ok, _Pid} = ocpp_station:start_link(StationID),
    StationID.

teardown_station(StationID) ->
    catch ocpp_station:stop(StationID),
    meck:unload(ocpp_station_manager).

setup_connected_station(StationID, InState) ->
    StationID = setup_station(StationID, []),
    Self = self(),
    Pid = spawn(
        fun() ->
            {ok, '2.0.1'} = ocpp_station:connect(StationID, ['2.0.1']),
            Loop = fun Conn(State) ->
                receive
                    stop ->
                        ok;
                    {goto, NewState} ->
                        establish_state(StationID, State, NewState),
                        Self ! ready,
                        Conn(State)
                end
            end,
            Loop(connected)
        end
    ),
    Pid ! {goto, InState},
    receive
        ready -> ok
    end,
    {StationID, Pid}.

teardown_connected_station({StationID, ConnPid}) ->
    case is_process_alive(ConnPid) of
        true ->
            ConnPid ! stop;
        false ->
            ok
    end,
    teardown_station(StationID).

establish_state(_, connected, connected) ->
    ok;
establish_state(StationID, State, NewState) ->
    error(not_implemented).

connect_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?withStation(
            <<"foo">>,
            [
                fun connect_invalid_station/1,
                fun connect_station/1,
                fun connect_station_again/1,
                fun connect_station_twice/1
            ]
        )}.

down_test_() ->
    %% TODO these tests should succeed no matter what state the station is in.
    {setup, fun setup_deps/0, fun teardown_deps/1, [
        ?withConnectedStation(
            list_to_binary(atom_to_list(?FUNCTION_NAME) ++ atom_to_list(State)),
            State,
            [
                fun reconnect_voluntary/1,
                fun reconnect_killed/1
            ]
        )
     || State <- [
            connected
            %% TODO None of these are supported yet.
            %% provisioning,
            %% boot_pending,
            %% provisioning_call_pending,
            %% accepted,
            %% call_pending,
            %% reconnecting
        ]
        %% TODO what should happen to the connection process if the station process exits?
    ]}.

connect_rejected_test_() ->
    {setup, fun setup_deps/0, fun teardown_deps/1,
        ?withStation(
            <<"connection_rejected_test">>,
            [
                fun connection_rejected/1,
                fun connection_handler_error/1
            ]
        )}.

connection_handler_error(StationID) ->
    {"connection fails when handler returns {error, _}", fun() ->
        meck:expect(ocpp_station_manager, connect, fun
            (_, ['1.6']) -> {error, because};
            (_, ['2.0.1']) -> {ok, '2.0.1'}
        end),
        ?assertEqual({error, because}, ocpp_station:connect(StationID, ['1.6'])),
        ?assertEqual({ok, '2.0.1'}, ocpp_station:connect(StationID, ['2.0.1'])),
        ?assert(meck:validate(ocpp_station_manager))
    end}.

connection_rejected(StationID) ->
    {"connection fails when rejected by handler", fun() ->
        %% ocpp_station_manager mock is created in setup. We just
        %% need to replace the connect expectation so it closes
        %% the connection.
        meck:expect(ocpp_station_manager, connect, fun
            (_, ['2.1']) -> close;
            (_, ['1.6']) -> {ok, '1.6'}
        end),
        ?assertEqual(close, ocpp_station:connect(StationID, ['2.1'])),
        ?assertEqual({ok, '1.6'}, ocpp_station:connect(StationID, ['1.6'])),
        ?assertEqual({error, already_connected}, ocpp_station:connect(StationID, ['1.6'])),
        ?assert(meck:validate(ocpp_station_manager))
    end}.

connect_invalid_station(_StationID) ->
    {"connecting to a station ID that does not exist fails",
        ?_assertError(nostation, ocpp_station:connect(<<"invalid">>, ['2.0.1']))}.

connect_station(StationID) ->
    {"can connect to a station", fun() ->
        {ok, '2.0.1'} = ocpp_station:connect(StationID, ['2.0.1'])
    end}.

connect_station_again(StationID) ->
    {"the same process can't connect to a station twice", fun() ->
        {ok, '2.1'} = ocpp_station:connect(StationID, ['1.6', '2.0.1', '2.1']),
        ?assertMatch({error, already_connected}, ocpp_station:connect(StationID, ['2.1']))
    end}.

connect_station_twice(StationID) ->
    {"two processes can't connect to the same station", fun() ->
        Self = self(),
        F = fun() ->
            Self ! ocpp_station:connect(StationID, ['1.6']),
            receive
                stop -> ok
            after 5000 -> error(timeout)
            end
        end,
        Pid1 = spawn_link(F),
        Pid2 = spawn_link(F),
        Messages = [
            receive
                M1 -> M1
            end,
            receive
                M2 -> M2
            end
        ],
        Pid1 ! Pid2 ! stop,
        ?assert(lists:member({ok, '1.6'}, Messages)),
        ?assert(lists:member({error, already_connected}, Messages))
    end}.

reconnect_voluntary({StationID, Pid}) ->
    {"different process can connect to station after original connection ends voluntarily", fun() ->
            true = link(Pid),
            process_flag(trap_exit, true),
            Pid ! stop,
            receive
                {'EXIT', Pid, normal} ->
                    ok
            end,
            ?assertEqual({ok, '2.0.1'}, ocpp_station:connect(StationID, ['2.0.1']))
        end}.

reconnect_killed({StationID, Pid}) ->
    {"different process can connect to station after original connection exits abnormally", fun() ->
            true = link(Pid),
            process_flag(trap_exit, true),
            exit(Pid, kill),
            receive
                {'EXIT', Pid, killed} ->
                    ok
            end,
            ?assertEqual({ok, '2.0.1'}, ocpp_station:connect(StationID, ['2.0.1']))
        end}.
