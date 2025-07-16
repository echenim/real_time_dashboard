%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Registry module for managing user ID to process PID mappings.
%%%
%%% This module provides the API for the presence registry ETS table.
%%% It handles lookups, insertions, and deletions of user presence data.
%%%
%%% Design Decision: Module API over GenServer
%%% Trade-off: Direct ETS access vs GenServer wrapper
%%% - Chosen: Direct ETS access for:
%%%   * Better performance (no message passing overhead)
%%%   * Simpler code (no gen_server boilerplate)
%%%   * ETS already handles concurrency well
%%% - Downside: Less control over access patterns
%%%
%%% The ETS table is owned by presence_registry_owner process.
%%% @end
%%%-------------------------------------------------------------------
-module(presence_registry).

%% API
-export([
    lookup/1,
    insert/2,
    delete/1,
    list_all/0,
    count/0,
    clear/0,
    get_user_info/1,
    update_last_seen/1,
    find_by_pid/1
]).

%% Types
-type user_id() :: binary().
-type error() :: {error, term()}.

%% Registry entry structure
%% Trade-off: Simple {UserId, Pid} vs rich record
%% - Chosen: Rich record for more flexibility and debugging info
-record(registry_entry, {
    user_id :: user_id(),
    pid :: pid(),
    node :: node(),
    started_at :: erlang:timestamp(),
    last_seen :: erlang:timestamp(),
    metadata :: map()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Look up a user's process PID by their ID.
%%
%% Time complexity: O(1) - ETS set lookup
%% @end
-spec lookup(UserId :: user_id()) -> {ok, pid()} | {error, not_found}.
lookup(UserId) when is_binary(UserId) ->
    %% Emit telemetry event for monitoring
    telemetry:execute([presence_core, registry, lookup], 
                      #{count => 1}, 
                      #{user_id => UserId}),
    
    case ets:lookup(presence_registry, UserId) of
        [{UserId, #registry_entry{pid = Pid}}] ->
            %% Verify the process is still alive
            %% Trade-off: Extra check vs stale data
            %% - Chosen: Check to ensure data integrity
            case is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    %% Process died but wasn't cleaned up yet
                    lager:warning("Found dead process for user ~s, cleaning up", [UserId]),
                    delete(UserId),
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc
%% Insert a new user-process mapping.
%%
%% Will overwrite any existing entry for the user.
%% Time complexity: O(1)
%% @end
-spec insert(UserId :: user_id(), Pid :: pid()) -> ok | error().
insert(UserId, Pid) when is_binary(UserId), is_pid(Pid) ->
    %% Create registry entry with metadata
    Entry = #registry_entry{
        user_id = UserId,
        pid = Pid,
        node = node(Pid),
        started_at = erlang:timestamp(),
        last_seen = erlang:timestamp(),
        metadata = #{}
    },
    
    %% Insert into ETS
    %% Trade-off: insert vs insert_new
    %% - Chosen: insert to allow overwrites (user reconnection)
    true = ets:insert(presence_registry, {UserId, Entry}),
    
    %% Emit telemetry
    telemetry:execute([presence_core, registry, insert], 
                      #{count => 1}, 
                      #{user_id => UserId, node => node(Pid)}),
    
    lager:debug("Registered user ~s with pid ~p", [UserId, Pid]),
    ok.

%% @doc
%% Delete a user from the registry.
%%
%% Time complexity: O(1)
%% @end
-spec delete(UserId :: user_id()) -> ok.
delete(UserId) when is_binary(UserId) ->
    %% Delete from ETS
    %% ets:delete/2 returns true regardless of whether key existed
    ets:delete(presence_registry, UserId),
    
    %% Emit telemetry
    telemetry:execute([presence_core, registry, delete], 
                      #{count => 1}, 
                      #{user_id => UserId}),
    
    lager:debug("Unregistered user ~s", [UserId]),
    ok.

%% @doc
%% List all registered users and their PIDs.
%%
%% Time complexity: O(n) where n is number of users
%% Use with caution on large datasets.
%% @end
-spec list_all() -> [{user_id(), pid()}].
list_all() ->
    %% Use ets:foldl for memory efficiency
    %% Trade-off: foldl vs tab2list
    %% - Chosen: foldl to avoid creating intermediate list
    ets:foldl(
        fun({UserId, #registry_entry{pid = Pid}}, Acc) ->
            %% Only include alive processes
            case is_process_alive(Pid) of
                true -> [{UserId, Pid} | Acc];
                false -> Acc
            end
        end,
        [],
        presence_registry
    ).

%% @doc
%% Count the number of registered users.
%%
%% Time complexity: O(1) - ETS maintains size
%% @end
-spec count() -> non_neg_integer().
count() ->
    ets:info(presence_registry, size).

%% @doc
%% Clear all entries from the registry.
%%
%% Use with caution! This is mainly for testing.
%% @end
-spec clear() -> ok.
clear() ->
    true = ets:delete_all_objects(presence_registry),
    lager:warning("Cleared all entries from presence registry"),
    ok.

%% @doc
%% Get detailed information about a user.
%%
%% Returns more info than simple lookup.
%% @end
-spec get_user_info(UserId :: user_id()) -> 
    {ok, map()} | {error, not_found}.
get_user_info(UserId) when is_binary(UserId) ->
    case ets:lookup(presence_registry, UserId) of
        [{UserId, #registry_entry{} = Entry}] ->
            %% Convert record to map for easier consumption
            Info = #{
                user_id => Entry#registry_entry.user_id,
                pid => Entry#registry_entry.pid,
                node => Entry#registry_entry.node,
                started_at => Entry#registry_entry.started_at,
                last_seen => Entry#registry_entry.last_seen,
                uptime_seconds => timer:now_diff(
                    erlang:timestamp(), 
                    Entry#registry_entry.started_at
                ) div 1000000,
                metadata => Entry#registry_entry.metadata
            },
            {ok, Info};
        [] ->
            {error, not_found}
    end.

%% @doc
%% Update the last seen timestamp for a user.
%%
%% Used for heartbeat tracking.
%% @end
-spec update_last_seen(UserId :: user_id()) -> ok | {error, not_found}.
update_last_seen(UserId) when is_binary(UserId) ->
    case ets:lookup(presence_registry, UserId) of
        [{UserId, Entry}] ->
            %% Update only the last_seen field
            NewEntry = Entry#registry_entry{last_seen = erlang:timestamp()},
            true = ets:insert(presence_registry, {UserId, NewEntry}),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc
%% Find user ID by process PID.
%%
%% Reverse lookup - useful for process monitoring.
%% Time complexity: O(n) - requires full table scan
%% @end
-spec find_by_pid(Pid :: pid()) -> {ok, user_id()} | {error, not_found}.
find_by_pid(Pid) when is_pid(Pid) ->
    %% Use ets:match_object for pattern matching
    case ets:match_object(presence_registry, {'_', #registry_entry{pid = Pid, _ = '_'}}) of
        [{UserId, _Entry}] ->
            {ok, UserId};
        [] ->
            {error, not_found};
        Multiple when length(Multiple) > 1 ->
            %% This shouldn't happen but handle it
            lager:error("Multiple users found for pid ~p: ~p", [Pid, Multiple]),
            {error, multiple_users}
    end.