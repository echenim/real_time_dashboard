%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for presence_tracker module.
%%% 
%%% Tests cover:
%%% - Basic status updates and retrieval
%%% - Heartbeat mechanism
%%% - Timeout handling
%%% - Disconnect behavior
%%% - Error conditions
%%% @end
%%%-------------------------------------------------------------------
-module(presence_tracker_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup/teardown
setup() ->
    %% Start required applications
    application:ensure_all_started(presence_core),
    
    %% Mock the presence_registry and presence_store modules
    meck:new(presence_registry, [non_strict]),
    meck:expect(presence_registry, register, fun(_, _) -> ok end),
    meck:expect(presence_registry, unregister, fun(_) -> ok end),
    
    meck:new(presence_store, [non_strict]),
    meck:expect(presence_store, update_presence, fun(_, _) -> ok end),
    
    ok.

teardown(_) ->
    meck:unload(presence_registry),
    meck:unload(presence_store),
    ok.

%% Test generator
presence_tracker_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Start and stop tracker", fun test_start_stop/0},
      {"Update status", fun test_update_status/0},
      {"Get status", fun test_get_status/0},
      {"Heartbeat mechanism", fun test_heartbeat/0},
      {"Heartbeat timeout", fun test_heartbeat_timeout/0},
      {"Disconnect", fun test_disconnect/0},
      {"Multiple status updates", fun test_multiple_status_updates/0},
      {"Concurrent operations", fun test_concurrent_operations/0}
     ]}.

%% Individual test cases

test_start_stop() ->
    UserId = <<"user123">>,
    {ok, Pid} = presence_tracker:start_link(UserId),
    ?assert(is_process_alive(Pid)),
    
    %% Verify registration was called
    ?assert(meck:called(presence_registry, register, [UserId, Pid])),
    
    %% Stop the process
    gen_server:stop(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)),
    
    %% Verify unregistration was called
    ?assert(meck:called(presence_registry, unregister, [UserId])).

test_update_status() ->
    UserId = <<"user456">>,
    {ok, Pid} = presence_tracker:start_link(UserId),
    
    %% Update to away status
    ok = presence_tracker:update_status(Pid, away),
    timer:sleep(50), % Allow cast to process
    
    %% Verify status was updated
    {ok, Status} = presence_tracker:get_status(Pid),
    ?assertEqual(away, maps:get(status, Status)),
    
    %% Update to busy status
    ok = presence_tracker:update_status(Pid, busy),
    timer:sleep(50),
    
    {ok, Status2} = presence_tracker:get_status(Pid),
    ?assertEqual(busy, maps:get(status, Status2)),
    
    %% Verify presence_store was notified
    ?assert(meck:called(presence_store, update_presence, ['_', '_'])),
    
    gen_server:stop(Pid).

test_get_status() ->
    UserId = <<"user789">>,
    {ok, Pid} = presence_tracker:start_link(UserId),
    
    %% Get initial status
    {ok, Status} = presence_tracker:get_status(Pid),
    
    %% Verify status structure
    ?assertEqual(UserId, maps:get(user_id, Status)),
    ?assertEqual(online, maps:get(status, Status)),
    ?assert(is_integer(maps:get(last_seen, Status))),
    ?assert(is_integer(maps:get(connected_at, Status))),
    ?assert(is_map(maps:get(metadata, Status))),
    
    gen_server:stop(Pid).

test_heartbeat() ->
    UserId = <<"user_heartbeat">>,
    {ok, Pid} = presence_tracker:start_link(UserId),
    
    %% Get initial last_seen
    {ok, Status1} = presence_tracker:get_status(Pid),
    LastSeen1 = maps:get(last_seen, Status1),
    
    %% Wait a bit
    timer:sleep(100),
    
    %% Send heartbeat
    ok = presence_tracker:heartbeat(Pid),
    timer:sleep(50),
    
    %% Verify last_seen was updated
    {ok, Status2} = presence_tracker:get_status(Pid),
    LastSeen2 = maps:get(last_seen, Status2),
    
    ?assert(LastSeen2 > LastSeen1),
    
    gen_server:stop(Pid).

test_heartbeat_timeout() ->
    %% This test requires mocking the timer to speed up the test
    UserId = <<"user_timeout">>,
    
    %% Start with very short timeout for testing
    %% Note: In real implementation, we'd make timeout configurable
    {ok, Pid} = presence_tracker:start_link(UserId),
    
    %% Simulate heartbeat timeout by sending the message directly
    Pid ! heartbeat_timeout,
    timer:sleep(50),
    
    %% Verify status changed to offline
    {ok, Status} = presence_tracker:get_status(Pid),
    ?assertEqual(offline, maps:get(status, Status)),
    
    %% Verify presence_store was notified
    ?assert(meck:called(presence_store, update_presence, [UserId, '_'])),
    
    gen_server:stop(Pid).

test_disconnect() ->
    UserId = <<"user_disconnect">>,
    {ok, Pid} = presence_tracker:start_link(UserId),
    
    %% Monitor the process to detect when it stops
    Ref = monitor(process, Pid),
    
    %% Disconnect
    ok = presence_tracker:disconnect(Pid),
    
    %% Wait for process to stop
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok
    after 1000 ->
        ?assert(false, "Process did not stop after disconnect")
    end,
    
    %% Verify final status update
    ?assert(meck:called(presence_store, update_presence, [UserId, '_'])).

test_multiple_status_updates() ->
    UserId = <<"user_multi">>,
    {ok, Pid} = presence_tracker:start_link(UserId),
    
    %% Perform multiple rapid status updates
    States = [away, busy, online, away, offline, online],
    
    lists:foreach(fun(State) ->
        ok = presence_tracker:update_status(Pid, State),
        timer:sleep(10)
    end, States),
    
    %% Final status should be online
    {ok, Status} = presence_tracker:get_status(Pid),
    ?assertEqual(online, maps:get(status, Status)),
    
    %% Verify multiple updates were processed
    CallCount = meck:num_calls(presence_store, update_presence, ['_', '_']),
    ?assert(CallCount >= length(States)),
    
    gen_server:stop(Pid).

test_concurrent_operations() ->
    UserId = <<"user_concurrent">>,
    {ok, Pid} = presence_tracker:start_link(UserId),
    
    %% Spawn multiple processes to perform concurrent operations
    Parent = self(),
    Workers = lists:map(fun(N) ->
        spawn(fun() ->
            case N rem 3 of
                0 ->
                    presence_tracker:update_status(Pid, away),
                    Parent ! {done, N};
                1 ->
                    presence_tracker:heartbeat(Pid),
                    Parent ! {done, N};
                2 ->
                    {ok, _} = presence_tracker:get_status(Pid),
                    Parent ! {done, N}
            end
        end)
    end, lists:seq(1, 30)),
    
    %% Wait for all workers to complete
    lists:foreach(fun(_) ->
        receive
            {done, _} -> ok
        after 1000 ->
            ?assert(false, "Worker timeout")
        end
    end, Workers),
    
    %% Verify process is still alive and responding
    ?assert(is_process_alive(Pid)),
    {ok, _Status} = presence_tracker:get_status(Pid),
    
    gen_server:stop(Pid).

%% Property-based tests would go here if using PropEr
%% Example properties to test:
%% - Status is always one of: online, offline, away, busy
%% - last_seen timestamp always increases
%% - Process always cleans up properly on termination