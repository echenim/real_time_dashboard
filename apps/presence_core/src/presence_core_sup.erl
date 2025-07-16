%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Top-level supervisor for the presence_core application.
%%%
%%% This supervisor manages the main components of the presence system:
%%% - presence_user_sup: Dynamic supervisor for user presence processes
%%% - presence_registry_owner: Process that owns the ETS registry
%%% - presence_cleanup: Process that handles timeout cleanup
%%%
%%% Design Decision: Using one_for_one restart strategy
%%% Trade-off: one_for_one vs one_for_all vs rest_for_one
%%% - Chosen: one_for_one because components are independent
%%% - If registry dies, users can re-register
%%% - If user supervisor dies, registry remains intact
%%%
%%% Supervision Tree Structure:
%%% presence_core_sup
%%% ├── presence_user_sup (dynamic supervisor)
%%% │   └── presence_user (GenServer per user)
%%% ├── presence_registry_owner (GenServer)
%%% └── presence_cleanup (GenServer)
%%% @end
%%%-------------------------------------------------------------------
-module(presence_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper functions
-export([start_user/1, stop_user/1, count_users/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts the top-level supervisor.
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc
%% Start a new user presence process.
%%
%% This is a convenience function that delegates to the dynamic supervisor.
%% Trade-off: Exposing through top supervisor vs direct access
%% - Chosen: Through top supervisor for better API organization
%% @end
-spec start_user(UserId :: binary()) -> {ok, pid()} | {error, term()}.
start_user(UserId) ->
    presence_user_sup:start_user(UserId).

%% @doc
%% Stop a user presence process.
%% @end
-spec stop_user(UserId :: binary()) -> ok | {error, not_found}.
stop_user(UserId) ->
    presence_user_sup:stop_user(UserId).

%% @doc
%% Count the number of active user processes.
%% @end
-spec count_users() -> non_neg_integer().
count_users() ->
    presence_user_sup:count_users().

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc
%% Initialize the supervisor with child specifications.
%%
%% Child specifications define how each child process should be
%% started, restarted, and shut down.
%% @end
-spec init(Args :: term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Supervisor flags
    %% - one_for_one: if a child dies, only restart that child
    %% - 10 restarts in 60 seconds max (prevent restart loops)
    %% Trade-off: Higher restart intensity vs system stability
    %% - Chosen: 10/60 as reasonable for production systems
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% Child specifications
    Children = [
        %% Registry owner process
        %% This process owns the ETS tables and ensures they survive
        %% if other processes crash
        #{
            id => presence_registry_owner,
            start => {presence_registry_owner, start_link, []},
            restart => permanent,  % Always restart
            shutdown => 5000,       % 5 seconds to cleanup
            type => worker,
            modules => [presence_registry_owner]
        },
        
        %% Dynamic supervisor for user processes
        %% Trade-off: simple_one_for_one vs dynamic supervisor
        %% - Chosen: dynamic supervisor (new OTP 24+ API) for better flexibility
        #{
            id => presence_user_sup,
            start => {presence_user_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,   % Give time to shutdown all children
            type => supervisor,
            modules => [presence_user_sup]
        },
        
        %% Cleanup process for handling timeouts
        %% This process periodically checks for timed-out users
        #{
            id => presence_cleanup,
            start => {presence_cleanup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [presence_cleanup]
        }
    ],
    
    {ok, {SupFlags, Children}}.