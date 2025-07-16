%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% presence_core OTP application callback module.
%%% 
%%% This module implements the application behavior and is responsible for
%%% starting and stopping the presence_core application.
%%%
%%% Design Decision: Using application behavior for proper OTP integration
%%% Trade-off: More boilerplate vs better supervision and lifecycle management
%%% - Chosen: OTP application pattern for production-grade reliability
%%%
%%% The application starts the top-level supervisor which manages all
%%% presence-related processes including:
%%% - User presence GenServers (one per active user)
%%% - Presence registry (ETS table owner)
%%% - Cleanup processes for handling timeouts
%%% @end
%%%-------------------------------------------------------------------
-module(presence_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Internal functions
-export([get_env/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc
%% Starts the presence_core application.
%% 
%% This function is called by the OTP application controller when
%% starting the application. It starts the top-level supervisor.
%%
%% Trade-off: Starting supervisor here vs manual process management
%% - Chosen: Supervisor for automatic restart and fault isolation
%% @end
-spec start(StartType, StartArgs) -> {ok, pid()} | {error, Reason} when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Reason :: term().
start(_StartType, _StartArgs) ->
    %% Log application start
    lager:info("Starting presence_core application"),
    
    %% Initialize ETS tables before supervisor starts
    %% Trade-off: Initialize here vs in supervisor
    %% - Chosen: Here to ensure tables exist before any process needs them
    ok = init_ets_tables(),
    
    %% Start telemetry events
    ok = init_telemetry(),
    
    %% Start the top-level supervisor
    case presence_core_sup:start_link() of
        {ok, Pid} ->
            lager:info("presence_core application started successfully"),
            {ok, Pid};
        {error, Reason} = Error ->
            lager:error("Failed to start presence_core application: ~p", [Reason]),
            Error
    end.

%% @doc
%% Stops the presence_core application.
%%
%% This function is called by the OTP application controller when
%% stopping the application. It performs cleanup operations.
%% @end
-spec stop(State :: term()) -> ok.
stop(_State) ->
    lager:info("Stopping presence_core application"),
    
    %% Cleanup operations
    %% Trade-off: Explicit cleanup vs letting processes die
    %% - Chosen: Explicit cleanup for graceful shutdown
    ok = cleanup_ets_tables(),
    
    %% Emit shutdown telemetry event
    telemetry:execute([presence_core, app, stop], #{count => 1}, #{}),
    
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Initialize ETS tables used by the application.
%%
%% Design Decision: Using ETS for in-memory storage
%% Trade-off: ETS vs Mnesia vs external DB
%% - Chosen: ETS for speed and simplicity in single-node deployments
%% - Mnesia would add complexity without clear benefits for this use case
%% - External DB would add latency for real-time operations
%% @end
-spec init_ets_tables() -> ok.
init_ets_tables() ->
    %% Registry table: maps user_id to process pid
    %% Options explained:
    %% - named_table: allows access by name instead of table ID
    %% - public: any process can read/write (controlled by API layer)
    %% - set: one value per key (vs bag/duplicate_bag)
    %% - {read_concurrency, true}: optimized for concurrent reads
    %% - {write_concurrency, true}: optimized for concurrent writes
    ets:new(presence_registry, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),
    
    %% Metrics table for storing aggregated statistics
    ets:new(presence_metrics, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    lager:info("ETS tables initialized successfully"),
    ok.

%% @private
%% @doc
%% Cleanup ETS tables on application shutdown.
%% @end
-spec cleanup_ets_tables() -> ok.
cleanup_ets_tables() ->
    %% Delete tables if they exist
    %% Using catch to handle case where tables don't exist
    catch ets:delete(presence_registry),
    catch ets:delete(presence_metrics),
    ok.

%% @private
%% @doc
%% Initialize telemetry events for monitoring.
%%
%% Design Decision: Using telemetry for metrics
%% Trade-off: Telemetry vs custom metrics system
%% - Chosen: Telemetry for standard Erlang ecosystem integration
%% @end
-spec init_telemetry() -> ok.
init_telemetry() ->
    %% Define telemetry events this application will emit
    Events = [
        [presence_core, user, online],
        [presence_core, user, offline],
        [presence_core, user, heartbeat],
        [presence_core, registry, lookup],
        [presence_core, registry, insert],
        [presence_core, registry, delete],
        [presence_core, app, start],
        [presence_core, app, stop]
    ],
    
    %% Log available events for debugging
    lager:info("Initialized telemetry events: ~p", [Events]),
    
    %% Emit startup event
    telemetry:execute([presence_core, app, start], #{count => 1}, #{}),
    
    ok.

%% @doc
%% Get application environment variable with default value.
%%
%% This is a helper function used throughout the application to
%% retrieve configuration values.
%% @end
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    application:get_env(presence_core, Key, Default).