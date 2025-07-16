%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% ETS backend implementation for presence storage.
%%%
%%% This module implements the presence_store behavior using ETS tables
%%% for in-memory storage. ETS provides fast access with good concurrency.
%%%
%%% Design Decisions:
%%% - Uses the existing presence_data table from presence_core
%%% - Stores full user data as maps for flexibility
%%% - No persistence (data lost on restart)
%%%
%%% Trade-offs:
%%% - Speed vs Persistence: Chosen speed for real-time requirements
%%% - Memory usage vs Disk I/O: Chosen memory for low latency
%%% - Single node vs Distributed: Single node for simplicity
%%%   (can add distribution layer later)
%%% @end
%%%-------------------------------------------------------------------
-module(presence_store_ets).

-behaviour(presence_store).

%% presence_store callbacks
-export([
    init/1,
    get_user/1,
    save_user/2,
    delete_user/1,
    list_users/0,
    count_users/0,
    bulk_get_users/1,
    bulk_save_users/1,
    bulk_delete_users/1,
    clear_all/0,
    get_stats/0
]).

%% Table name for storing user data
-define(TABLE, presence_data).

%% Record for storing user data
%% Using a record for better performance than pure maps
-record(user_record, {
    user_id :: binary(),
    data :: map(),
    updated_at :: integer()
}).

%%%===================================================================
%%% presence_store callbacks
%%%===================================================================

%% @doc
%% Initialize the ETS backend.
%%
%% Creates the table if it doesn't exist.
%% @end
-spec init(Opts :: map()) -> ok | {error, term()}.
init(Opts) ->
    %% Check if table already exists
    case ets:info(?TABLE, name) of
        undefined ->
            %% Create table with options from config
            TableOpts = maps:get(ets_options, Opts, default_table_opts()),
            
            try
                ets:new(?TABLE, TableOpts),
                lager:info("Created ETS table ~p for presence storage", [?TABLE]),
                ok
            catch
                error:badarg ->
                    %% Table might exist with different options
                    {error, {table_creation_failed, ?TABLE}}
            end;
        ?TABLE ->
            %% Table already exists
            lager:debug("ETS table ~p already exists", [?TABLE]),
            ok
    end.

%% @doc
%% Get user data from ETS.
%%
%% Time complexity: O(1)
%% @end
-spec get_user(UserId :: binary()) -> {ok, map()} | {error, not_found} | {error, term()}.
get_user(UserId) ->
    case ets:lookup(?TABLE, UserId) of
        [#user_record{data = Data}] ->
            {ok, Data};
        [] ->
            {error, not_found};
        Invalid ->
            lager:error("Invalid data in ETS for user ~s: ~p", [UserId, Invalid]),
            {error, corrupted_data}
    end.

%% @doc
%% Save user data to ETS.
%%
%% Time complexity: O(1)
%% @end
-spec save_user(UserId :: binary(), Data :: map()) -> ok | {error, term()}.
save_user(UserId, Data) ->
    Record = #user_record{
        user_id = UserId,
        data = Data,
        updated_at = erlang:system_time(millisecond)
    },
    
    try
        true = ets:insert(?TABLE, Record),
        ok
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% Delete user from ETS.
%%
%% Time complexity: O(1)
%% @end
-spec delete_user(UserId :: binary()) -> ok | {error, term()}.
delete_user(UserId) ->
    try
        ets:delete(?TABLE, UserId),
        ok
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% List all users.
%%
%% Time complexity: O(n) where n is number of users
%% Memory: Creates list of all users - use with caution
%% @end
-spec list_users() -> {ok, [map()]} | {error, term()}.
list_users() ->
    try
        Users = ets:foldl(
            fun(#user_record{data = Data}, Acc) ->
                [Data | Acc]
            end,
            [],
            ?TABLE
        ),
        {ok, Users}
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% Count users in ETS.
%%
%% Time complexity: O(1) - ETS maintains size
%% @end
-spec count_users() -> {ok, non_neg_integer()} | {error, term()}.
count_users() ->
    try
        Size = ets:info(?TABLE, size),
        {ok, Size}
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% Bulk get users from ETS.
%%
%% More efficient than multiple lookups due to better cache locality.
%% Time complexity: O(k) where k is number of requested users
%% @end
-spec bulk_get_users(UserIds :: [binary()]) -> {ok, [map()]} | {error, term()}.
bulk_get_users(UserIds) ->
    try
        Users = lists:foldl(
            fun(UserId, Acc) ->
                case ets:lookup(?TABLE, UserId) of
                    [#user_record{data = Data}] ->
                        [Data | Acc];
                    [] ->
                        Acc  % Skip missing users
                end
            end,
            [],
            UserIds
        ),
        {ok, lists:reverse(Users)}
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% Bulk save users to ETS.
%%
%% Uses single insert operation for efficiency.
%% Time complexity: O(k) where k is number of users
%% @end
-spec bulk_save_users(Users :: [{binary(), map()}]) -> ok | {error, term()}.
bulk_save_users(Users) ->
    Timestamp = erlang:system_time(millisecond),
    Records = [
        #user_record{
            user_id = UserId,
            data = Data,
            updated_at = Timestamp
        } || {UserId, Data} <- Users
    ],
    
    try
        true = ets:insert(?TABLE, Records),
        ok
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% Bulk delete users from ETS.
%%
%% Trade-off: Individual deletes vs match_delete
%% - Chosen: Individual for simplicity and predictable performance
%% @end
-spec bulk_delete_users(UserIds :: [binary()]) -> ok | {error, term()}.
bulk_delete_users(UserIds) ->
    try
        lists:foreach(
            fun(UserId) ->
                ets:delete(?TABLE, UserId)
            end,
            UserIds
        ),
        ok
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% Clear all data from ETS.
%%
%% DANGEROUS: Removes all user data
%% Time complexity: O(n) but very fast
%% @end
-spec clear_all() -> ok | {error, term()}.
clear_all() ->
    try
        true = ets:delete_all_objects(?TABLE),
        lager:warning("Cleared all data from ETS table ~p", [?TABLE]),
        ok
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%% @doc
%% Get storage statistics.
%%
%% Provides insights into storage usage and performance.
%% @end
-spec get_stats() -> {ok, map()} | {error, term()}.
get_stats() ->
    try
        Info = ets:info(?TABLE),
        
        %% Extract relevant statistics
        Stats = #{
            backend => ets,
            table_name => ?TABLE,
            size => proplists:get_value(size, Info, 0),
            memory_bytes => proplists:get_value(memory, Info, 0) * erlang:system_info(wordsize),
            owner => proplists:get_value(owner, Info),
            protection => proplists:get_value(protection, Info),
            type => proplists:get_value(type, Info),
            keypos => proplists:get_value(keypos, Info),
            read_concurrency => proplists:get_value(read_concurrency, Info),
            write_concurrency => proplists:get_value(write_concurrency, Info)
        },
        
        {ok, Stats}
    catch
        error:badarg ->
            {error, table_not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Default table options if not specified in config.
%%
%% Design choices:
%% - set: One value per key (no duplicates)
%% - public: Any process can access (controlled at API level)
%% - named_table: Access by name instead of table ID
%% - {keypos, 2}: Key is in position 2 of the record
%% - Concurrency options for better multi-core performance
%% @end
-spec default_table_opts() -> [term()].
default_table_opts() ->
    [
        named_table,
        public,
        set,
        {keypos, #user_record.user_id},
        {read_concurrency, true},
        {write_concurrency, true}
    ].