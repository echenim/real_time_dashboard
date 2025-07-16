%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Unit tests for presence_store_ets module.
%%%
%%% Tests verify all storage operations including:
%%% - CRUD operations
%%% - Bulk operations
%%% - Error handling
%%% - Performance characteristics
%%% @end
%%%-------------------------------------------------------------------
-module(presence_store_ets_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test setup/teardown
setup() ->
    %% Ensure table doesn't exist
    catch ets:delete(presence_data),
    
    %% Initialize the backend
    ok = presence_store_ets:init(#{}),
    ok.

cleanup(_) ->
    %% Delete the table
    catch ets:delete(presence_data),
    ok.

%% Test generator
presence_store_ets_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_init/0,
      fun test_save_and_get/0,
      fun test_delete/0,
      fun test_list_users/0,
      fun test_count_users/0,
      fun test_bulk_operations/0,
      fun test_clear_all/0,
      fun test_get_stats/0,
      fun test_concurrent_operations/0,
      fun test_error_cases/0,
      fun test_data_integrity/0
     ]}.

%% Test cases

test_init() ->
    %% Table should exist after init
    ?assertNotEqual(undefined, ets:info(presence_data, name)),
    
    %% Second init should succeed (idempotent)
    ?assertEqual(ok, presence_store_ets:init(#{})).

test_save_and_get() ->
    UserId = <<"test_user">>,
    UserData = #{
        status => online,
        ip => <<"192.168.1.1">>,
        metadata => #{client => web}
    },
    
    %% Save user
    ?assertEqual(ok, presence_store_ets:save_user(UserId, UserData)),
    
    %% Get user
    {ok, Retrieved} = presence_store_ets:get_user(UserId),
    ?assertEqual(online, maps:get(status, Retrieved)),
    ?assertEqual(<<"192.168.1.1">>, maps:get(ip, Retrieved)),
    ?assertMatch(#{client := web}, maps:get(metadata, Retrieved)),
    
    %% Get non-existent user
    ?assertEqual({error, not_found}, presence_store_ets:get_user(<<"unknown">>)).

test_delete() ->
    UserId = <<"delete_test">>,
    UserData = #{status => online},
    
    %% Save and verify
    presence_store_ets:save_user(UserId, UserData),
    ?assertMatch({ok, _}, presence_store_ets:get_user(UserId)),
    
    %% Delete
    ?assertEqual(ok, presence_store_ets:delete_user(UserId)),
    
    %% Verify deleted
    ?assertEqual({error, not_found}, presence_store_ets:get_user(UserId)),
    
    %% Delete non-existent (should not error)
    ?assertEqual(ok, presence_store_ets:delete_user(<<"nonexistent">>)).

test_list_users() ->
    %% Empty list initially
    ?assertEqual({ok, []}, presence_store_ets:list_users()),
    
    %% Add users
    Users = [
        {<<"user1">>, #{name => <<"Alice">>, status => online}},
        {<<"user2">>, #{name => <<"Bob">>, status => away}},
        {<<"user3">>, #{name => <<"Charlie">>, status => online}}
    ],
    
    lists:foreach(fun({Id, Data}) ->
        presence_store_ets:save_user(Id, Data)
    end, Users),
    
    %% List all
    {ok, AllUsers} = presence_store_ets:list_users(),
    ?assertEqual(3, length(AllUsers)),
    
    %% Verify all users are present (order not guaranteed)
    Names = [maps:get(name, U) || U <- AllUsers],
    ?assert(lists:member(<<"Alice">>, Names)),
    ?assert(lists:member(<<"Bob">>, Names)),
    ?assert(lists:member(<<"Charlie">>, Names)).

test_count_users() ->
    %% Initially empty
    ?assertEqual({ok, 0}, presence_store_ets:count_users()),
    
    %% Add users incrementally
    presence_store_ets:save_user(<<"user1">>, #{}),
    ?assertEqual({ok, 1}, presence_store_ets:count_users()),
    
    presence_store_ets:save_user(<<"user2">>, #{}),
    ?assertEqual({ok, 2}, presence_store_ets:count_users()),
    
    %% Update existing user (count shouldn't change)
    presence_store_ets:save_user(<<"user1">>, #{updated => true}),
    ?assertEqual({ok, 2}, presence_store_ets:count_users()),
    
    %% Delete user
    presence_store_ets:delete_user(<<"user1">>),
    ?assertEqual({ok, 1}, presence_store_ets:count_users()).

test_bulk_operations() ->
    %% Prepare test data
    Users = [
        {<<"bulk1">>, #{name => <<"User1">>}},
        {<<"bulk2">>, #{name => <<"User2">>}},
        {<<"bulk3">>, #{name => <<"User3">>}},
        {<<"bulk4">>, #{name => <<"User4">>}},
        {<<"bulk5">>, #{name => <<"User5">>}}
    ],
    UserIds = [Id || {Id, _} <- Users],
    
    %% Bulk save
    ?assertEqual(ok, presence_store_ets:bulk_save_users(Users)),
    
    %% Verify all saved
    ?assertEqual({ok, 5}, presence_store_ets:count_users()),
    
    %% Bulk get
    {ok, Retrieved} = presence_store_ets:bulk_get_users(UserIds),
    ?assertEqual(5, length(Retrieved)),
    
    %% Bulk get with some missing users
    MixedIds = [<<"bulk1">>, <<"missing1">>, <<"bulk3">>, <<"missing2">>],
    {ok, Mixed} = presence_store_ets:bulk_get_users(MixedIds),
    ?assertEqual(2, length(Mixed)),  % Only existing users returned
    
    %% Bulk delete
    DeleteIds = [<<"bulk1">>, <<"bulk3">>, <<"bulk5">>],
    ?assertEqual(ok, presence_store_ets:bulk_delete_users(DeleteIds)),
    
    %% Verify deletions
    ?assertEqual({ok, 2}, presence_store_ets:count_users()),
    ?assertEqual({error, not_found}, presence_store_ets:get_user(<<"bulk1">>)),
    ?assertMatch({ok, _}, presence_store_ets:get_user(<<"bulk2">>)).

test_clear_all() ->
    %% Add test data
    lists:foreach(fun(N) ->
        UserId = list_to_binary("clear_test_" ++ integer_to_list(N)),
        presence_store_ets:save_user(UserId, #{index => N})
    end, lists:seq(1, 10)),
    
    ?assertEqual({ok, 10}, presence_store_ets:count_users()),
    
    %% Clear all
    ?assertEqual(ok, presence_store_ets:clear_all()),
    
    %% Verify empty
    ?assertEqual({ok, 0}, presence_store_ets:count_users()),
    ?assertEqual({ok, []}, presence_store_ets:list_users()).

test_get_stats() ->
    %% Add some data
    lists:foreach(fun(N) ->
        UserId = list_to_binary("stats_user_" ++ integer_to_list(N)),
        UserData = #{
            name => list_to_binary("User " ++ integer_to_list(N)),
            metadata => #{index => N}
        },
        presence_store_ets:save_user(UserId, UserData)
    end, lists:seq(1, 5)),
    
    %% Get stats
    {ok, Stats} = presence_store_ets:get_stats(),
    
    %% Verify stats structure
    ?assertEqual(ets, maps:get(backend, Stats)),
    ?assertEqual(presence_data, maps:get(table_name, Stats)),
    ?assertEqual(5, maps:get(size, Stats)),
    ?assert(maps:get(memory_bytes, Stats) > 0),
    ?assertEqual(public, maps:get(protection, Stats)),
    ?assertEqual(set, maps:get(type, Stats)),
    ?assertEqual(true, maps:get(read_concurrency, Stats)),
    ?assertEqual(true, maps:get(write_concurrency, Stats)).

test_concurrent_operations() ->
    %% Test concurrent writes
    Parent = self(),
    NumProcesses = 100,
    
    %% Spawn processes to write concurrently
    lists:foreach(fun(N) ->
        spawn(fun() ->
            UserId = list_to_binary("concurrent_" ++ integer_to_list(N)),
            UserData = #{process => N, pid => self()},
            presence_store_ets:save_user(UserId, UserData),
            Parent ! {done, N}
        end)
    end, lists:seq(1, NumProcesses)),
    
    %% Wait for all writes
    lists:foreach(fun(N) ->
        receive
            {done, N} -> ok
        after 5000 ->
            ?assert(false)  % Timeout
        end
    end, lists:seq(1, NumProcesses)),
    
    %% Verify all writes succeeded
    ?assertEqual({ok, NumProcesses}, presence_store_ets:count_users()),
    
    %% Test concurrent reads
    lists:foreach(fun(N) ->
        spawn(fun() ->
            UserId = list_to_binary("concurrent_" ++ integer_to_list(N)),
            {ok, Data} = presence_store_ets:get_user(UserId),
            ?assertEqual(N, maps:get(process, Data)),
            Parent ! {read, N}
        end)
    end, lists:seq(1, NumProcesses)),
    
    %% Wait for all reads
    lists:foreach(fun(N) ->
        receive
            {read, N} -> ok
        after 5000 ->
            ?assert(false)  % Timeout
        end
    end, lists:seq(1, NumProcesses)).

test_error_cases() ->
    %% Delete the table to simulate errors
    ets:delete(presence_data),
    
    %% All operations should return appropriate errors
    ?assertEqual({error, table_not_found}, presence_store_ets:get_user(<<"any">>)),
    ?assertEqual({error, table_not_found}, presence_store_ets:save_user(<<"any">>, #{})),
    ?assertEqual({error, table_not_found}, presence_store_ets:delete_user(<<"any">>)),
    ?assertEqual({error, table_not_found}, presence_store_ets:list_users()),
    ?assertEqual({error, table_not_found}, presence_store_ets:count_users()),
    ?assertEqual({error, table_not_found}, presence_store_ets:clear_all()),
    ?assertEqual({error, table_not_found}, presence_store_ets:get_stats()).

test_data_integrity() ->
    %% Test that updates preserve data integrity
    UserId = <<"integrity_test">>,
    
    %% Initial data
    InitialData = #{
        status => online,
        ip => <<"192.168.1.1">>,
        metadata => #{browser => <<"chrome">>, version => 95}
    },
    presence_store_ets:save_user(UserId, InitialData),
    
    %% Update with new data
    UpdatedData = #{
        status => away,
        ip => <<"192.168.1.1">>,
        metadata => #{browser => <<"chrome">>, version => 96, new_field => true}
    },
    presence_store_ets:save_user(UserId, UpdatedData),
    
    %% Verify complete replacement (not merge)
    {ok, Retrieved} = presence_store_ets:get_user(UserId),
    ?assertEqual(away, maps:get(status, Retrieved)),
    ?assertEqual(<<"192.168.1.1">>, maps:get(ip, Retrieved)),
    ?assertEqual(96, maps:get(version, maps:get(metadata, Retrieved))),
    ?assertEqual(true, maps:get(new_field, maps:get(metadata, Retrieved))).

%% Performance characteristics test (commented out for normal test runs)
%% Uncomment to test performance
% performance_test() ->
%     setup(),
%     
%     %% Measure write performance
%     WriteStart = erlang:monotonic_time(microsecond),
%     lists:foreach(fun(N) ->
%         UserId = list_to_binary("perf_" ++ integer_to_list(N)),
%         presence_store_ets:save_user(UserId, #{index => N})
%     end, lists:seq(1, 10000)),
%     WriteTime = erlang:monotonic_time(microsecond) - WriteStart,
%     
%     io:format("Write 10k users: ~p microseconds (~p/user)~n", 
%               [WriteTime, WriteTime / 10000]),
%     
%     %% Measure read performance
%     ReadStart = erlang:monotonic_time(microsecond),
%     lists:foreach(fun(N) ->
%         UserId = list_to_binary("perf_" ++ integer_to_list(N)),
%         {ok, _} = presence_store_ets:get_user(UserId)
%     end, lists:seq(1, 10000)),
%     ReadTime = erlang:monotonic_time(microsecond) - ReadStart,
%     
%     io:format("Read 10k users: ~p microseconds (~p/user)~n", 
%               [ReadTime, ReadTime / 10000]),
%     
%     cleanup(ok).

%% Additional edge cases

unicode_data_test() ->
    setup(),
    UserId = <<"unicode_user">>,
    UserData = #{
        name => <<"用户123"/utf8>>,
        status => <<"在线"/utf8>>,
        emoji => <<"🚀🎉"/utf8>>
    },
    
    ?assertEqual(ok, presence_store_ets:save_user(UserId, UserData)),
    {ok, Retrieved} = presence_store_ets:get_user(UserId),
    ?assertEqual(UserData, Retrieved),
    
    cleanup(ok).

large_data_test() ->
    setup(),
    UserId = <<"large_data_user">>,
    
    %% Create large metadata
    LargeData = #{
        binary_data => binary:copy(<<"x">>, 10000),
        nested_map => maps:from_list([{N, N*N} || N <- lists:seq(1, 1000)])
    },
    
    ?assertEqual(ok, presence_store_ets:save_user(UserId, LargeData)),
    {ok, Retrieved} = presence_store_ets:get_user(UserId),
    ?assertEqual(10000, byte_size(maps:get(binary_data, Retrieved))),
    ?assertEqual(1000, map_size(maps:get(nested_map, Retrieved))),
    
    cleanup(ok).