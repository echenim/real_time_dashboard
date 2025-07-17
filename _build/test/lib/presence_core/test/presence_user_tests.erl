%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Unit tests for presence_user GenServer.
%%%
%%% Tests verify:
%%% - User process lifecycle
%%% - Heartbeat handling
%%% - Timeout behavior
%%% - Metadata management
%%% - State queries
%%% @end
%%%-------------------------------------------------------------------
-module(presence_user_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test timeout values (shorter for faster tests)
-define(TEST_HEARTBEAT_INTERVAL, 100).  % 100ms
-define(TEST_OFFLINE_TIMEOUT, 300).     % 300ms

%% Setup/teardown
setup() ->
    %% Start required applications
    application:ensure_all_started(lager),
    
    %% Set test configuration
    application:set_env(presence_core, heartbeat_interval, ?TEST_HEARTBEAT_INTERVAL),
    application:set_env(presence_core, offline_timeout, ?TEST_OFFLINE_TIMEOUT),
    
    %% Ensure registry exists
    catch ets:new(presence_registry, [
        named_table, public, set,
        {keypos, 1},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    ok.

cleanup(_) ->
    %% Clear registry
    catch ets:delete_all_objects(presence_registry),
    ok.

%% Test generator
presence_user_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_start_and_register/0,
      fun test_heartbeat/0,
      fun test_timeout/0,
      fun test_metadata/0,
      fun test_get_state/0,
      fun test_disconnect/0,
      fun test_concurrent_heartbeats/0,
      fun test_process_crash/0
     ]}.

%% Test cases

test_start_and_register() ->
    UserId = <<"test_user_1">>,
    
    %% Start user process
    {ok, Pid} = presence_user:start_link(UserId),
    ?assert(is_process_alive(Pid)),
    
    %% Verify registration
    ?assertEqual({ok, Pid}, presence_registry:lookup(UserId)),
    
    %% Stop process
    presence_user:disconnect(Pid),
    timer:sleep(50),
    
    %% Verify unregistered
    ?assertEqual({error, not_found}, presence_registry:lookup(UserId)).

test_heartbeat() ->
    UserId = <<"heartbeat_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Get initial state
    {ok, State1} = presence_user:get_state(Pid),
    InitialHeartbeat = maps:get(last_heartbeat, State1),
    
    %% Send heartbeat
    timer:sleep(50),
    ok = presence_user:heartbeat(Pid),
    
    %% Verify heartbeat updated
    {ok, State2} = presence_user:get_state(Pid),
    NewHeartbeat = maps:get(last_heartbeat, State2),
    ?assert(NewHeartbeat > InitialHeartbeat),
    
    %% Test heartbeat by user ID
    timer:sleep(50),
    ok = presence_user:heartbeat(UserId),
    
    %% Cleanup
    presence_user:disconnect(Pid).

test_timeout() ->
    UserId = <<"timeout_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Monitor the process
    MonitorRef = monitor(process, Pid),
    
    %% Wait for timeout (should happen after TEST_OFFLINE_TIMEOUT)
    receive
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            ?assertEqual({shutdown, timeout}, Reason),
            %% Verify unregistered
            ?assertEqual({error, not_found}, presence_registry:lookup(UserId))
    after ?TEST_OFFLINE_TIMEOUT + 200 ->
        %% Should have timed out by now
        ?assert(false)
    end.

test_metadata() ->
    UserId = <<"metadata_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Initial metadata should be empty
    {ok, State1} = presence_user:get_state(Pid),
    ?assertEqual(#{}, maps:get(metadata, State1)),
    
    %% Update metadata
    Metadata1 = #{ip => <<"192.168.1.1">>, client => <<"web">>},
    ok = presence_user:update_metadata(Pid, Metadata1),
    
    %% Verify metadata
    {ok, State2} = presence_user:get_state(Pid),
    ?assertEqual(Metadata1, maps:get(metadata, State2)),
    
    %% Update with additional fields (should merge)
    Metadata2 = #{version => <<"1.0.0">>},
    ok = presence_user:update_metadata(UserId, Metadata2),  % Test by user ID
    
    %% Verify merged metadata
    {ok, State3} = presence_user:get_state(Pid),
    ExpectedMetadata = maps:merge(Metadata1, Metadata2),
    ?assertEqual(ExpectedMetadata, maps:get(metadata, State3)),
    
    %% Cleanup
    presence_user:disconnect(Pid).

test_get_state() ->
    UserId = <<"state_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Get state
    {ok, State} = presence_user:get_state(Pid),
    
    %% Verify state structure
    ?assertEqual(UserId, maps:get(user_id, State)),
    ?assertEqual(online, maps:get(status, State)),
    ?assert(maps:is_key(connected_at, State)),
    ?assert(maps:is_key(last_heartbeat, State)),
    ?assert(maps:is_key(uptime_seconds, State)),
    ?assert(maps:is_key(metadata, State)),
    
    %% Uptime should be non-negative
    ?assert(maps:get(uptime_seconds, State) >= 0),
    
    %% Test get_state by user ID
    {ok, State2} = presence_user:get_state(UserId),
    ?assertEqual(State, State2),
    
    %% Cleanup
    presence_user:disconnect(Pid).

test_disconnect() ->
    UserId = <<"disconnect_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Monitor the process
    MonitorRef = monitor(process, Pid),
    
    %% Disconnect
    ok = presence_user:disconnect(Pid),
    
    %% Should terminate normally
    receive
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    after 1000 ->
        ?assert(false)
    end,
    
    %% Verify unregistered
    ?assertEqual({error, not_found}, presence_registry:lookup(UserId)).

test_concurrent_heartbeats() ->
    UserId = <<"concurrent_heartbeat_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Send multiple heartbeats concurrently
    Parent = self(),
    NumHeartbeats = 100,
    
    lists:foreach(fun(N) ->
        spawn(fun() ->
            presence_user:heartbeat(Pid),
            Parent ! {heartbeat_sent, N}
        end)
    end, lists:seq(1, NumHeartbeats)),
    
    %% Wait for all heartbeats
    lists:foreach(fun(N) ->
        receive
            {heartbeat_sent, N} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, lists:seq(1, NumHeartbeats)),
    
    %% Process should still be alive
    ?assert(is_process_alive(Pid)),
    
    %% Cleanup
    presence_user:disconnect(Pid).

test_process_crash() ->
    UserId = <<"crash_test_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Kill the process ungracefully
    exit(Pid, kill),
    timer:sleep(50),
    
    %% Verify it was removed from registry
    ?assertEqual({error, not_found}, presence_registry:lookup(UserId)).

%% Additional edge case tests

invalid_operations_test() ->
    %% Test operations on non-existent users
    ?assertEqual({error, not_found}, presence_user:heartbeat(<<"nonexistent">>)),
    ?assertEqual({error, not_found}, presence_user:get_state(<<"nonexistent">>)),
    ?assertEqual({error, not_found}, presence_user:update_metadata(<<"nonexistent">>, #{})),
    ?assertEqual({error, not_found}, presence_user:disconnect(<<"nonexistent">>)).

status_transitions_test() ->
    setup(),
    UserId = <<"status_test_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Initially online
    {ok, State1} = presence_user:get_state(Pid),
    ?assertEqual(online, maps:get(status, State1)),
    
    %% Wait for one heartbeat interval without sending heartbeat
    %% Status should change to away
    timer:sleep(?TEST_HEARTBEAT_INTERVAL + 50),
    {ok, State2} = presence_user:get_state(Pid),
    ?assertEqual(away, maps:get(status, State2)),
    
    %% Send heartbeat to go back online
    presence_user:heartbeat(Pid),
    {ok, State3} = presence_user:get_state(Pid),
    ?assertEqual(online, maps:get(status, State3)),
    
    %% Cleanup
    presence_user:disconnect(Pid),
    cleanup(ok).

long_running_user_test() ->
    %% Test that a user with regular heartbeats stays online
    setup(),
    UserId = <<"long_running_user">>,
    {ok, Pid} = presence_user:start_link(UserId),
    
    %% Send heartbeats for several intervals
    lists:foreach(fun(_) ->
        timer:sleep(?TEST_HEARTBEAT_INTERVAL div 2),
        presence_user:heartbeat(Pid),
        %% Verify still online
        {ok, State} = presence_user:get_state(Pid),
        ?assertEqual(online, maps:get(status, State))
    end, lists:seq(1, 10)),
    
    %% Cleanup
    presence_user:disconnect(Pid),
    cleanup(ok).