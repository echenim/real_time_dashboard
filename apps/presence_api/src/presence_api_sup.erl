%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Top-level supervisor for the presence_api application.
%%%
%%% This supervisor manages API-related processes such as:
%%% - WebSocket connection registry
%%% - Rate limiting processes
%%% - Metrics aggregation
%%% @end
%%%-------------------------------------------------------------------
-module(presence_api_sup).

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
%% Initialize the supervisor with child specifications.
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
    
    %% Child specifications
    Children = [
        %% WebSocket connection registry
        %% Tracks active WebSocket connections for broadcasting
        #{
            id => ws_registry,
            start => {presence_api_ws_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [presence_api_ws_registry]
        },
        
        %% Metrics aggregator
        %% Collects and aggregates API metrics
        #{
            id => metrics_aggregator,
            start => {presence_api_metrics_aggregator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [presence_api_metrics_aggregator]
        }
        
        %% Note: Rate limiting uses ETS tables and doesn't need a process
    ],
    
    {ok, {SupFlags, Children}}.