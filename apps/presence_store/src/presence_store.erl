%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Behavior definition and API for presence storage backends.
%%%
%%% This module defines the interface that all storage backends must
%%% implement. It also provides the public API that automatically
%%% delegates to the configured backend.
%%%
%%% Design Decision: Behavior-based abstraction
%%% Trade-off: Runtime dispatch vs compile-time selection
%%% - Chosen: Runtime dispatch for:
%%%   * Hot-swappable backends
%%%   * Easier testing with mock backends
%%%   * Configuration-based backend selection
%%% - Downside: Small performance overhead for dispatch
%%%
%%% All backends must implement:
%%% - Basic CRUD operations for user presence
%%% - Bulk operations for efficiency
%%% - Query operations for analytics
%%% - Maintenance operations
%%% @end
%%%-------------------------------------------------------------------
-module(presence_store).

%% Behavior definition
-callback init(Opts :: map()) -> ok | {error, term()}.
-callback get_user(UserId :: binary()) -> {ok, map()} | {error, not_found} | {error, term()}.
-callback save_user(UserId :: binary(), Data :: map()) -> ok | {error, term()}.
-callback delete_user(UserId :: binary()) -> ok | {error, term()}.
-callback list_users() -> {ok, [map()]} | {error, term()}.
-callback count_users() -> {ok, non_neg_integer()} | {error, term()}.
-callback bulk_get_users(UserIds :: [binary()]) -> {ok, [map()]} | {error, term()}.
-callback bulk_save_users(Users :: [{binary(), map()}]) -> ok | {error, term()}.
-callback bulk_delete_users(UserIds :: [binary()]) -> ok | {error, term()}.
-callback clear_all() -> ok | {error, term()}.
-callback get_stats() -> {ok, map()} | {error, term()}.

%% Optional callbacks for advanced features
-optional_callbacks([
    query_users/1,        % Advanced queries
    subscribe/1,          % Real-time change notifications
    unsubscribe/1,
    backup/1,            % Backup/restore operations
    restore/1
]).

%% Public API
-export([
    init/0,
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

%% Types
-export_type([user_id/0, user_data/0, error/0]).

-type user_id() :: binary().
-type user_data() :: map().
-type error() :: {error, term()}.

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc
%% Initialize the storage backend with default options.
%% @end
-spec init() -> ok | error().
init() ->
    init(#{}).

%% @doc
%% Initialize the storage backend with options.
%% @end
-spec init(Opts :: map()) -> ok | error().
init(Opts) ->
    Backend = presence_store_app:get_backend_module(),
    
    %% Add backend-specific options from config
    BackendOpts = get_backend_opts(Opts),
    
    case Backend:init(BackendOpts) of
        ok ->
            lager:info("Storage backend ~p initialized", [Backend]),
            ok;
        {error, Reason} = Error ->
            lager:error("Failed to initialize backend ~p: ~p", [Backend, Reason]),
            Error
    end.

%% @doc
%% Get user presence data.
%%
%% Time complexity varies by backend:
%% - ETS: O(1)
%% - Redis: O(1) + network latency
%% - PostgreSQL: O(log n) + network latency
%% @end
-spec get_user(UserId :: user_id()) -> {ok, user_data()} | {error, not_found} | error().
get_user(UserId) ->
    Backend = presence_store_app:get_backend_module(),
    
    %% Input validation
    case validate_user_id(UserId) of
        ok ->
            %% Measure operation time
            StartTime = erlang:monotonic_time(microsecond),
            Result = Backend:get_user(UserId),
            Duration = erlang:monotonic_time(microsecond) - StartTime,
            
            %% Emit telemetry
            telemetry:execute(
                [presence_store, get_user],
                #{duration => Duration},
                #{backend => Backend, found => element(1, Result) =:= ok}
            ),
            
            Result;
        Error ->
            Error
    end.

%% @doc
%% Save user presence data.
%%
%% This will create or update the user data.
%% @end
-spec save_user(UserId :: user_id(), Data :: user_data()) -> ok | error().
save_user(UserId, Data) ->
    Backend = presence_store_app:get_backend_module(),
    
    %% Input validation
    case validate_user_data(UserId, Data) of
        ok ->
            %% Add timestamp if not present
            DataWithTimestamp = ensure_timestamp(Data),
            
            %% Measure operation time
            StartTime = erlang:monotonic_time(microsecond),
            Result = Backend:save_user(UserId, DataWithTimestamp),
            Duration = erlang:monotonic_time(microsecond) - StartTime,
            
            %% Emit telemetry
            telemetry:execute(
                [presence_store, save_user],
                #{duration => Duration},
                #{backend => Backend, success => Result =:= ok}
            ),
            
            Result;
        Error ->
            Error
    end.

%% @doc
%% Delete user presence data.
%% @end
-spec delete_user(UserId :: user_id()) -> ok | error().
delete_user(UserId) ->
    Backend = presence_store_app:get_backend_module(),
    
    case validate_user_id(UserId) of
        ok ->
            Backend:delete_user(UserId);
        Error ->
            Error
    end.

%% @doc
%% List all users.
%%
%% Use with caution on large datasets.
%% Consider using pagination in production.
%% @end
-spec list_users() -> {ok, [user_data()]} | error().
list_users() ->
    Backend = presence_store_app:get_backend_module(),
    Backend:list_users().

%% @doc
%% Count total users.
%% @end
-spec count_users() -> {ok, non_neg_integer()} | error().
count_users() ->
    Backend = presence_store_app:get_backend_module(),
    Backend:count_users().

%% @doc
%% Bulk get users.
%%
%% More efficient than multiple get_user calls.
%% @end
-spec bulk_get_users(UserIds :: [user_id()]) -> {ok, [user_data()]} | error().
bulk_get_users(UserIds) when is_list(UserIds) ->
    Backend = presence_store_app:get_backend_module(),
    
    %% Validate all user IDs
    case lists:all(fun(Id) -> validate_user_id(Id) =:= ok end, UserIds) of
        true ->
            Backend:bulk_get_users(UserIds);
        false ->
            {error, invalid_user_ids}
    end.

%% @doc
%% Bulk save users.
%%
%% More efficient than multiple save_user calls.
%% @end
-spec bulk_save_users(Users :: [{user_id(), user_data()}]) -> ok | error().
bulk_save_users(Users) when is_list(Users) ->
    Backend = presence_store_app:get_backend_module(),
    
    %% Add timestamps to all user data
    UsersWithTimestamp = [{UserId, ensure_timestamp(Data)} || {UserId, Data} <- Users],
    
    Backend:bulk_save_users(UsersWithTimestamp).

%% @doc
%% Bulk delete users.
%% @end
-spec bulk_delete_users(UserIds :: [user_id()]) -> ok | error().
bulk_delete_users(UserIds) when is_list(UserIds) ->
    Backend = presence_store_app:get_backend_module(),
    Backend:bulk_delete_users(UserIds).

%% @doc
%% Clear all data.
%%
%% DANGEROUS: Use only for testing or with explicit admin action.
%% @end
-spec clear_all() -> ok | error().
clear_all() ->
    Backend = presence_store_app:get_backend_module(),
    lager:warning("Clearing all data from storage backend ~p", [Backend]),
    Backend:clear_all().

%% @doc
%% Get storage statistics.
%% @end
-spec get_stats() -> {ok, map()} | error().
get_stats() ->
    Backend = presence_store_app:get_backend_module(),
    Backend:get_stats().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Get backend-specific options from configuration.
%% @end
-spec get_backend_opts(BaseOpts :: map()) -> map().
get_backend_opts(BaseOpts) ->
    Backend = presence_store_app:get_backend(),
    
    %% Get backend-specific config
    BackendConfig = case Backend of
        ets ->
            application:get_env(presence_store, ets_options, []);
        redis ->
            application:get_env(presence_store, redis_options, []);
        postgres ->
            application:get_env(presence_store, postgres_options, []);
        _ ->
            []
    end,
    
    %% Merge with base options
    maps:merge(maps:from_list(BackendConfig), BaseOpts).

%% @private
%% @doc
%% Validate user ID.
%% @end
-spec validate_user_id(UserId :: term()) -> ok | {error, invalid_user_id}.
validate_user_id(UserId) when is_binary(UserId), byte_size(UserId) > 0 ->
    ok;
validate_user_id(_) ->
    {error, invalid_user_id}.

%% @private
%% @doc
%% Validate user data.
%% @end
-spec validate_user_data(UserId :: term(), Data :: term()) -> ok | {error, term()}.
validate_user_data(UserId, Data) when is_map(Data) ->
    validate_user_id(UserId);
validate_user_data(_, _) ->
    {error, invalid_user_data}.

%% @private
%% @doc
%% Ensure data has a timestamp.
%% @end
-spec ensure_timestamp(Data :: user_data()) -> user_data().
ensure_timestamp(Data) ->
    case maps:is_key(timestamp, Data) of
        true -> Data;
        false -> Data#{timestamp => erlang:system_time(millisecond)}
    end.