%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests for the presence_core application.
%%% 
%%% These tests verify the complete functionality of the presence
%%% tracking system including:
%%% - Application startup and shutdown
%%% - User registration and tracking
%%% - Heartbeat mechanism
%%% - Timeout handling
%%% - Registry consistency
%%% - Supervisor fault tolerance
%%% @end
%%%-------------------------------------------------------------------
-module(presence_core_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup/teardown
setup() ->
    %% Start the presence_core application
    {ok, _} = application:ensure_all_started(presence_core),
    ok.

teardown(_) ->
    %% Stop the application
    application:stop(presence_core),
    %% Give processes time to clean up
    timer:sleep(100),
    ok.

%% Test generator
presence_core_integration_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Application lifecycle", fun test_application_lifecycle/0},
      {"User lifecycle", fun test_user_lifecycle/0},
      {"Multiple users", fun test_multiple_users/0},
      {"Heartbeat mechanism", fun test_heartbeat_mechanism/0},
      {"Timeout behavior", fun test_timeout_behavior/0},
      {"Concurrent user registration", fun test_concurrent_registration/0},
      {"Supervisor restart", fun test_supervisor_restart/0},
      {"Registry consistency", fun test_registry_consistency/0},
      {"Cleanup process", fun test_cleanup_process/0},
      {"Performance under load", fun test_performance_under_load/0}
     ]}.

%% Individual test cases

test_application_lifecycle() ->
    %% Verify application is running
    ?assertEqual(ok, application:ensure_started(presence_core)),
    
    %% Check that supervisors are running
    ?assert(is_process_alive(whereis(presence_core_sup))),
    ?assert(is_process_alive(whereis(presence_tracker_sup))),
    ?assert(is_process_alive(whereis(presence_registry_owner))),
    ?assert(is_process_alive(whereis(presence_cleanup))),
    
    %% Verify ETS tables exist
    ?assertNotEqual(undefined, ets:info(presence_registry)),
    ?assertNotEqual(undefined, ets:info(presence_metrics)).

test_user_lifecycle() ->
    UserId = <<"test_user_lifecycle">>,
    
    %% Start user tracker
    {ok, Pid} = presence_tracker_sup:start_tracker(UserId),
    ?assert(is_process_alive(Pid)),
    
    %% Verify registration
    ?assertEqual({ok, Pid}, presence_registry:lookup(UserId)),
    
    %% Get status
    {ok, Status} = presence_tracker:get_status(Pid),
    ?assertEqual(UserId, maps:get(user_id, Status)),
    ?assertEqual(online, maps:get(status, Status)),
    
    %% Update status
    ok = presence_tracker:update_status(Pid, away),
    {ok, Status2} = presence_tracker:get_status(Pid),
    ?assertEqual(away, maps:get(status, Status2)),
    
    %% Disconnect
    ok = presence_tracker:disconnect(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)),
    
    %% Verify cleanup
    ?assertEqual({error, not_found}, presence_registry:lookup(UserId)).

test_multiple_users() ->
    UserIds = [<<"user_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 10)],
    
    %% Start all users
    Pids = lists:map(fun(UserId) ->
        {ok, Pid} = presence_tracker_sup:start_tracker(UserId),
        Pid
    end, UserIds),
    
    %% Verify all are registered
    ?assertEqual(10, presence_registry:count()),
    
    %% Update some statuses
    lists:foreach(fun({Pid, N}) ->
        Status = case N rem 3 of
            0 -> away;
            1 -> busy;
            _ -> online
        end,
        ok = presence_tracker:update_status(Pid, Status)
    end, lists:zip(Pids, lists:seq(1, 10))),
    
    %% Verify mixed statuses
    AllUsers = presence_registry:get_all_users(),
    ?assertEqual(10, length(AllUsers)),
    
    %% Stop all users
    lists:foreach(fun(Pid) ->
        presence_tracker:disconnect(Pid)
    end, Pids),
    
    timer:sleep(200),
    
    %% Verify all cleaned up
    ?assertEqual(0, presence_registry:count()).

test_heartbeat_mechanism() ->
    UserId = <<"test_heartbeat">>,
    
    %% Start user with custom presence_user (which has heartbeat timeout)
    {ok, _Pid} = presence_user_sup:start_user(UserId),
    
    %% Initial state
    {ok, State1} = presence_user:get_state(UserId),
    LastHeartbeat1 = maps:get(last_heartbeat, State1),
    
    %% Send heartbeats
    lists:foreach(fun(_) ->
        timer:sleep(100),
        ok = presence_user:heartbeat(UserId)
    end, lists:seq(1, 5)),
    
    %% Verify heartbeat was updated
    {ok, State2} = presence_user:get_state(UserId),
    LastHeartbeat2 = maps:get(last_heartbeat, State2),
    ?assert(LastHeartbeat2 > LastHeartbeat1),
    
    %% Cleanup
    presence_user:disconnect(UserId).

test_timeout_behavior() ->
    %% This test would require mocking the timeout or using very short timeouts
    %% For now, we'll test the mechanism directly
    UserId = <<"test_timeout">>,
    
    {ok, Pid} = presence_tracker_sup:start_tracker(UserId),
    
    %% Simulate timeout by sending the timeout message directly
    Pid ! heartbeat_timeout,
    timer:sleep(100),
    
    %% Check status changed to offline
    case presence_tracker:get_status(Pid) of
        {ok, Status} ->
            ?assertEqual(offline, maps:get(status, Status));
        {error, _} ->
            %% Process might have already died
            ok
    end.

test_concurrent_registration() ->
    %% Test concurrent registration of many users
    NumUsers = 100,
    Parent = self(),
    
    %% Spawn processes to register users concurrently
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            UserId = <<"concurrent_", (integer_to_binary(N))/binary>>,
            case presence_tracker_sup:start_tracker(UserId) of
                {ok, TrackerPid} ->
                    Parent ! {registered, N, TrackerPid};
                {error, Reason} ->
                    Parent ! {failed, N, Reason}
            end
        end)
    end, lists:seq(1, NumUsers)),
    
    %% Collect results
    Results = lists:map(fun(_) ->
        receive
            {registered, N, Pid} -> {ok, N, Pid};
            {failed, N, Reason} -> {error, N, Reason}
        after 5000 ->
            {error, timeout}
        end
    end, Pids),
    
    %% Verify all succeeded
    OkResults = [R || {ok, _, _} = R <- Results],
    ?assertEqual(NumUsers, length(OkResults)),
    
    %% Verify count
    ActualCount = presence_registry:count(),
    ?assert(ActualCount >= NumUsers),
    
    %% Cleanup
    lists:foreach(fun({ok, _, Pid}) ->
        catch presence_tracker:disconnect(Pid)
    end, OkResults).

test_supervisor_restart() ->
    UserId = <<"test_supervisor_restart">>,
    
    %% Start a user
    {ok, Pid1} = presence_tracker_sup:start_tracker(UserId),
    
    %% Kill the process abnormally
    exit(Pid1, kill),
    timer:sleep(100),
    
    %% Supervisor should not restart it (transient strategy)
    ?assertEqual({error, not_found}, presence_registry:lookup(UserId)),
    
    %% Start again
    {ok, Pid2} = presence_tracker_sup:start_tracker(UserId),
    ?assert(is_process_alive(Pid2)),
    ?assertNotEqual(Pid1, Pid2),
    
    %% Cleanup
    presence_tracker:disconnect(Pid2).

test_registry_consistency() ->
    %% Test that registry stays consistent through various operations
    UserIds = [<<"consistency_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 20)],
    
    %% Start half the users
    FirstHalf = lists:sublist(UserIds, 10),
    lists:foreach(fun(UserId) ->
        {ok, _} = presence_tracker_sup:start_tracker(UserId)
    end, FirstHalf),
    
    ?assertEqual(10, presence_registry:count()),
    
    %% Kill some processes
    lists:foreach(fun(UserId) ->
        case presence_registry:lookup(UserId) of
            {ok, Pid} -> exit(Pid, kill);
            _ -> ok
        end
    end, lists:sublist(FirstHalf, 5)),
    
    timer:sleep(100),
    
    %% Registry should be cleaned up
    ?assert(presence_registry:count() =< 5),
    
    %% Start the second half
    SecondHalf = lists:nthtail(10, UserIds),
    lists:foreach(fun(UserId) ->
        {ok, _} = presence_tracker_sup:start_tracker(UserId)
    end, SecondHalf),
    
    %% Verify count
    FinalCount = presence_registry:count(),
    ?assert(FinalCount >= 10),
    ?assert(FinalCount =< 15).

test_cleanup_process() ->
    %% Force a cleanup cycle
    {ok, Stats1} = presence_cleanup:get_stats(),
    InitialCleanupCount = maps:get(cleanup_count, Stats1),
    
    %% Create some dead processes
    lists:foreach(fun(N) ->
        UserId = <<"cleanup_test_", (integer_to_binary(N))/binary>>,
        Pid = spawn(fun() -> ok end),
        timer:sleep(10), % Ensure process is dead
        
        %% Manually insert into registry (simulating orphaned entry)
        ets:insert(presence_registry, {UserId, {registry_entry, UserId, Pid, node(), 
                                                erlang:timestamp(), erlang:timestamp(), #{}}})
    end, lists:seq(1, 5)),
    
    %% Trigger cleanup
    {ok, _} = presence_cleanup:force_cleanup(),
    
    %% Verify cleanup happened
    {ok, Stats2} = presence_cleanup:get_stats(),
    ?assertEqual(InitialCleanupCount + 1, maps:get(cleanup_count, Stats2)).

test_performance_under_load() ->
    %% Simple performance test
    NumUsers = 500,
    
    %% Measure registration time
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Register users in batches
    lists:foreach(fun(Batch) ->
        lists:foreach(fun(N) ->
            UserId = <<"perf_user_", (integer_to_binary(N))/binary>>,
            {ok, _} = presence_tracker_sup:start_tracker(UserId)
        end, lists:seq((Batch-1)*50 + 1, Batch*50))
    end, lists:seq(1, 10)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Verify all registered
    ?assertEqual(NumUsers, presence_registry:count()),
    
    %% Log performance
    ?debugFmt("Registered ~p users in ~p ms (~p users/sec)", 
              [NumUsers, Duration, (NumUsers * 1000) div Duration]),
    
    %% Performance assertion - should handle at least 100 users/sec
    ?assert((NumUsers * 1000) div Duration >= 100),
    
    %% Cleanup - disconnect all users
    AllUsers = presence_registry:get_all_users(),
    lists:foreach(fun(UserId) ->
        case presence_registry:lookup(UserId) of
            {ok, Pid} -> catch presence_tracker:disconnect(Pid);
            _ -> ok
        end
    end, AllUsers),
    
    timer:sleep(500),
    ?assertEqual(0, presence_registry:count()).