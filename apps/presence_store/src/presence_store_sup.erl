%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Top-level supervisor for the presence_store application.
%%%
%%% This supervisor manages storage backend processes if needed.
%%% For ETS backend, there are no child processes since ETS tables
%%% are managed by presence_core. For Redis/PostgreSQL, this would
%%% supervise connection pools.
%%%
%%% Design Decision: Empty supervisor for ETS vs populated for others
%%% Trade-off: Conditional children vs always empty
%%% - Chosen: Dynamic children based on backend for flexibility
%%% @end
%%%-------------------------------------------------------------------
-module(presence_store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts the supervisor.
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc
%% Initialize the supervisor.
%%
%% Child specifications depend on the configured backend.
%% @end
-spec init(Args :: term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% Get backend-specific children
    Backend = presence_store_app:get_backend(),
    Children = get_children_for_backend(Backend),
    
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Get child specifications based on backend.
%% @end
-spec get_children_for_backend(Backend :: atom()) -> [supervisor:child_spec()].
get_children_for_backend(ets) ->
    %% ETS backend doesn't need any supervised processes
    %% Tables are owned by presence_registry_owner in presence_core
    [];

get_children_for_backend(redis) ->
    %% Redis backend would have connection pool here
    %% Example structure (not implemented):
    % [
    %     #{
    %         id => redis_pool,
    %         start => {eredis_pool, start_link, [
    %             [
    %                 {name, {local, presence_redis_pool}},
    %                 {size, 10},
    %                 {max_overflow, 20},
    %                 {host, "localhost"},
    %                 {port, 6379}
    %             ]
    %         ]},
    %         restart => permanent,
    %         shutdown => 5000,
    %         type => worker,
    %         modules => [eredis_pool]
    %     }
    % ]
    [];

get_children_for_backend(postgres) ->
    %% PostgreSQL backend would have connection pool here
    %% Example structure (not implemented):
    % [
    %     #{
    %         id => postgres_pool,
    %         start => {epgsql_pool, start_link, [
    %             presence_postgres_pool,
    %             [
    %                 {size, 10},
    %                 {max_overflow, 20},
    %                 {host, "localhost"},
    %                 {port, 5432},
    %                 {database, "presence_db"},
    %                 {username, "presence_user"}
    %             ]
    %         ]},
    %         restart => permanent,
    %         shutdown => 5000,
    %         type => worker,
    %         modules => [epgsql_pool]
    %     }
    % ]
    [];

get_children_for_backend(Backend) ->
    lager:warning("Unknown backend ~p, no children to start", [Backend]),
    [].