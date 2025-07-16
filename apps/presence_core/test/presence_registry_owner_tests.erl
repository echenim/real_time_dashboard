%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Unit tests for presence_registry_owner module.
%%%
%%% Tests verify:
%%% - Process startup and initialization
%%% - Table ownership management
%%% - Statistics retrieval
%%% - Health check functionality
%%% @end
%%%-------------------------------------------------------------------
-module(presence_registry_owner_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup/teardown
setup() ->
    %% Start required applications
    application:ensure_all_started(lager),
    
    %% Create ETS tables if they don't exist
    catch ets:new(presence_registry, [named_table, public, set]),
    catch ets:new(presence_metrics, [named_table, public, set]),
    
    %% Start the registry owner
    {ok, Pid} = presence_registry_owner:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the process
    gen_server:stop(Pid),
    
    %% Clean up ETS tables
    catch ets:delete(presence_registry),
    catch ets:delete(presence_metrics),
    ok.

%% Test generator
presence_registry_owner_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_startup/1,
      fun test_table_ownership/1,
      fun test_get_stats/1,
      fun test_health_check/1,
      fun test_process_exit_handling/1
     ]}.

%% Individual test cases

test_startup(Pid) ->
    fun() ->
        %% Verify process is alive
        ?assert(is_process_alive(Pid)),
        
        %% Verify it's registered locally
        ?assertEqual(Pid, whereis(presence_registry_owner)),
        
        %% Verify process info
        {status, _, _, _} = sys:get_status(Pid),
        ?assert(true)  % If we got here, process is responding
    end.

test_table_ownership(_Pid) ->
    fun() ->
        %% Verify the registry owner owns the tables
        Self = whereis(presence_registry_owner),
        
        %% Check presence_registry ownership
        ?assertEqual(Self, ets:info(presence_registry, owner)),
        
        %% Check presence_metrics ownership  
        ?assertEqual(Self, ets:info(presence_metrics, owner)),
        
        %% Verify tables are public (required for ownership transfer)
        ?assertEqual(public, ets:info(presence_registry, protection)),
        ?assertEqual(public, ets:info(presence_metrics, protection))
    end.

test_get_stats(Pid) ->
    fun() ->
        %% Insert some test data
        ets:insert(presence_registry, {<<"user1">>, self()}),
        ets:insert(presence_registry, {<<"user2">>, self()}),
        ets:insert(presence_metrics, {active_users, 2}),
        
        %% Get statistics
        {ok, Stats} = gen_server:call(Pid, get_stats),
        
        %% Verify stats structure
        ?assertMatch(#{
            tables := [presence_registry, presence_metrics],
            uptime_seconds := _,
            registry_size := 2,
            metrics_size := 1,
            memory := _
        }, Stats),
        
        %% Verify uptime is positive
        #{uptime_seconds := Uptime} = Stats,
        ?assert(Uptime >= 0),
        
        %% Verify memory info exists
        #{memory := Memory} = Stats,
        ?assertEqual(2, length(Memory))
    end.

test_health_check(Pid) ->
    fun() ->
        %% Trigger health check manually
        Pid ! health_check,
        
        %% Give it time to process
        timer:sleep(100),
        
        %% Verify process is still alive
        ?assert(is_process_alive(Pid)),
        
        %% Verify tables still exist and are owned correctly
        ?assertEqual(Pid, ets:info(presence_registry, owner)),
        ?assertEqual(Pid, ets:info(presence_metrics, owner))
    end.

test_process_exit_handling(Pid) ->
    fun() ->
        %% Send a fake EXIT signal
        Pid ! {'EXIT', self(), normal},
        
        %% Give it time to process
        timer:sleep(100),
        
        %% Verify process handled it gracefully
        ?assert(is_process_alive(Pid))
    end.

%% Additional edge case tests

ownership_transfer_test() ->
    %% This test verifies table ownership can be transferred
    %% Start by creating a table owned by the test process
    TableName = test_transfer_table,
    ets:new(TableName, [named_table, public, set]),
    
    %% Verify we own it
    ?assertEqual(self(), ets:info(TableName, owner)),
    
    %% Start registry owner
    {ok, Pid} = presence_registry_owner:start_link(),
    
    %% Transfer ownership
    ets:give_away(TableName, Pid, transfer),
    
    %% Verify ownership transferred
    ?assertEqual(Pid, ets:info(TableName, owner)),
    
    %% Cleanup
    gen_server:stop(Pid),
    catch ets:delete(TableName),
    ok.

unknown_request_handling_test() ->
    {ok, Pid} = presence_registry_owner:start_link(),
    
    %% Test unknown call
    ?assertEqual({error, unknown_request}, 
                 gen_server:call(Pid, unknown_request)),
    
    %% Test unknown cast (should not crash)
    gen_server:cast(Pid, unknown_cast),
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    
    %% Test unknown info (should not crash)
    Pid ! unknown_info,
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    
    %% Cleanup
    gen_server:stop(Pid),
    ok.