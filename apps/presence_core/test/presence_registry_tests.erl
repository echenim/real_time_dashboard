%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Unit tests for presence_registry module.
%%%
%%% Tests verify registry operations including:
%%% - User lookup and insertion
%%% - Deletion and clearing
%%% - Edge cases and error handling
%%% @end
%%%-------------------------------------------------------------------
-module(presence_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test setup/teardown
setup() ->
    %% Ensure the ETS table exists
    catch ets:new(presence_registry, [
        named_table, public, set,
        {keypos, 1},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    %% Clear any existing data
    presence_registry:clear(),
    ok.

cleanup(_) ->
    %% Clear the registry after each test
    presence_registry:clear(),
    ok.

%% Test generator
presence_registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_insert_and_lookup/0,
      fun test_delete/0,
      fun test_list_all/0,
      fun test_count/0,
      fun test_clear/0,
      fun test_get_user_info/0,
      fun test_update_last_seen/0,
      fun test_find_by_pid/0,
      fun test_dead_process_cleanup/0,
      fun test_concurrent_operations/0
     ]}.

%% Test cases

test_insert_and_lookup() ->
    UserId = <<"user123">>,
    Pid = self(),
    
    %% Insert user
    ?assertEqual(ok, presence_registry:insert(UserId, Pid)),
    
    %% Lookup should return the PID
    ?assertEqual({ok, Pid}, presence_registry:lookup(UserId)),
    
    %% Non-existent user should return error
    ?assertEqual({error, not_found}, presence_registry:lookup(<<"unknown">>)).

test_delete() ->
    UserId = <<"user456">>,
    Pid = self(),
    
    %% Insert and verify
    presence_registry:insert(UserId, Pid),
    ?assertEqual({ok, Pid}, presence_registry:lookup(UserId)),
    
    %% Delete
    ?assertEqual(ok, presence_registry:delete(UserId)),
    
    %% Verify deletion
    ?assertEqual({error, not_found}, presence_registry:lookup(UserId)),
    
    %% Delete non-existent user should not error
    ?assertEqual(ok, presence_registry:delete(<<"nonexistent">>)).

test_list_all() ->
    %% Start with empty registry
    ?assertEqual([], presence_registry:list_all()),
    
    %% Add multiple users
    Users = [
        {<<"user1">>, self()},
        {<<"user2">>, self()},
        {<<"user3">>, self()}
    ],
    
    lists:foreach(fun({UserId, Pid}) ->
        presence_registry:insert(UserId, Pid)
    end, Users),
    
    %% List all should return all users
    AllUsers = presence_registry:list_all(),
    ?assertEqual(3, length(AllUsers)),
    
    %% Verify all users are present
    lists:foreach(fun({UserId, Pid}) ->
        ?assert(lists:member({UserId, Pid}, AllUsers))
    end, Users).

test_count() ->
    %% Empty registry
    ?assertEqual(0, presence_registry:count()),
    
    %% Add users
    presence_registry:insert(<<"user1">>, self()),
    ?assertEqual(1, presence_registry:count()),
    
    presence_registry:insert(<<"user2">>, self()),
    ?assertEqual(2, presence_registry:count()),
    
    %% Overwrite existing user
    presence_registry:insert(<<"user1">>, self()),
    ?assertEqual(2, presence_registry:count()),
    
    %% Delete user
    presence_registry:delete(<<"user1">>),
    ?assertEqual(1, presence_registry:count()).

test_clear() ->
    %% Add multiple users
    lists:foreach(fun(N) ->
        UserId = list_to_binary("user" ++ integer_to_list(N)),
        presence_registry:insert(UserId, self())
    end, lists:seq(1, 5)),
    
    ?assertEqual(5, presence_registry:count()),
    
    %% Clear all
    ?assertEqual(ok, presence_registry:clear()),
    
    %% Verify empty
    ?assertEqual(0, presence_registry:count()),
    ?assertEqual([], presence_registry:list_all()).

test_get_user_info() ->
    UserId = <<"testuser">>,
    Pid = self(),
    
    %% Insert user
    presence_registry:insert(UserId, Pid),
    
    %% Wait a bit to have measurable uptime
    timer:sleep(100),
    
    %% Get user info
    {ok, Info} = presence_registry:get_user_info(UserId),
    
    %% Verify info structure
    ?assertEqual(UserId, maps:get(user_id, Info)),
    ?assertEqual(Pid, maps:get(pid, Info)),
    ?assertEqual(node(), maps:get(node, Info)),
    ?assert(maps:is_key(started_at, Info)),
    ?assert(maps:is_key(last_seen, Info)),
    ?assert(maps:is_key(uptime_seconds, Info)),
    ?assert(maps:is_key(metadata, Info)),
    
    %% Uptime should be positive
    ?assert(maps:get(uptime_seconds, Info) >= 0),
    
    %% Non-existent user
    ?assertEqual({error, not_found}, presence_registry:get_user_info(<<"unknown">>)).

test_update_last_seen() ->
    UserId = <<"heartbeat_user">>,
    presence_registry:insert(UserId, self()),
    
    %% Get initial info
    {ok, Info1} = presence_registry:get_user_info(UserId),
    LastSeen1 = maps:get(last_seen, Info1),
    
    %% Wait and update
    timer:sleep(100),
    ?assertEqual(ok, presence_registry:update_last_seen(UserId)),
    
    %% Verify last_seen was updated
    {ok, Info2} = presence_registry:get_user_info(UserId),
    LastSeen2 = maps:get(last_seen, Info2),
    
    ?assert(LastSeen2 > LastSeen1),
    
    %% Update non-existent user
    ?assertEqual({error, not_found}, presence_registry:update_last_seen(<<"unknown">>)).

test_find_by_pid() ->
    Pid = self(),
    UserId = <<"reverse_lookup_user">>,
    
    %% Insert user
    presence_registry:insert(UserId, Pid),
    
    %% Find by PID
    ?assertEqual({ok, UserId}, presence_registry:find_by_pid(Pid)),
    
    %% Non-existent PID
    FakePid = spawn(fun() -> ok end),
    ?assertEqual({error, not_found}, presence_registry:find_by_pid(FakePid)).

test_dead_process_cleanup() ->
    UserId = <<"dead_process_user">>,
    
    %% Spawn a process that will die
    Pid = spawn(fun() -> ok end),
    timer:sleep(50), % Ensure process is dead
    
    %% Insert with dead process
    presence_registry:insert(UserId, Pid),
    
    %% Lookup should detect dead process and clean up
    ?assertEqual({error, not_found}, presence_registry:lookup(UserId)),
    
    %% Verify it was cleaned up
    ?assertEqual(0, presence_registry:count()).

test_concurrent_operations() ->
    %% Test concurrent inserts
    NumProcesses = 100,
    Parent = self(),
    
    %% Spawn processes that will register themselves
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            UserId = list_to_binary("concurrent_user_" ++ integer_to_list(N)),
            presence_registry:insert(UserId, self()),
            Parent ! {registered, N}
        end)
    end, lists:seq(1, NumProcesses)),
    
    %% Wait for all registrations
    lists:foreach(fun(N) ->
        receive
            {registered, N} -> ok
        after 1000 ->
            ?assert(false) % Timeout
        end
    end, lists:seq(1, NumProcesses)),
    
    %% Verify all were registered
    ?assertEqual(NumProcesses, presence_registry:count()),
    
    %% Cleanup
    presence_registry:clear().

%% Property-based tests would go here if using PropEr

%% Edge cases

unicode_userid_test() ->
    %% Test with unicode user IDs
    setup(),
    UserId = <<"用户123"/utf8>>,
    presence_registry:insert(UserId, self()),
    ?assertEqual({ok, self()}, presence_registry:lookup(UserId)),
    cleanup(ok).

large_userid_test() ->
    %% Test with very large user ID
    setup(),
    UserId = binary:copy(<<"a">>, 10000),
    presence_registry:insert(UserId, self()),
    ?assertEqual({ok, self()}, presence_registry:lookup(UserId)),
    cleanup(ok).

overwrite_test() ->
    %% Test overwriting existing entry
    setup(),
    UserId = <<"overwrite_user">>,
    Pid1 = spawn(fun() -> timer:sleep(1000) end),
    Pid2 = spawn(fun() -> timer:sleep(1000) end),
    
    %% First insert
    presence_registry:insert(UserId, Pid1),
    ?assertEqual({ok, Pid1}, presence_registry:lookup(UserId)),
    
    %% Overwrite with new PID
    presence_registry:insert(UserId, Pid2),
    ?assertEqual({ok, Pid2}, presence_registry:lookup(UserId)),
    
    %% Count should still be 1
    ?assertEqual(1, presence_registry:count()),
    
    cleanup(ok).