%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% presence_api OTP application callback module.
%%%
%%% This application provides HTTP and WebSocket API endpoints for
%%% the presence system. It uses Cowboy as the web server.
%%%
%%% Design Decision: Cowboy for HTTP/WebSocket server
%%% Trade-off: Cowboy vs other servers (Elli, Mochiweb, Yaws)
%%% - Chosen: Cowboy for:
%%%   * Excellent WebSocket support
%%%   * HTTP/2 capability
%%%   * Active development and community
%%%   * Good performance characteristics
%%% @end
%%%-------------------------------------------------------------------
-module(presence_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([get_env/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc
%% Starts the presence_api application.
%%
%% Sets up Cowboy HTTP server with routing.
%% @end
-spec start(StartType, StartArgs) -> {ok, pid()} | {error, Reason} when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Reason :: term().
start(_StartType, _StartArgs) ->
    lager:info("Starting presence_api application"),
    
    %% Compile and configure routes
    Dispatch = compile_routes(),
    
    %% Get server configuration
    Port = get_env(http_port, 8080),
    Acceptors = get_env(http_acceptors, 100),
    
    %% Start Cowboy listener
    %% Trade-off: clear_port vs ssl
    %% - Starting with clear_port for development
    %% - Production should use ssl with proper certificates
    TransportOpts = #{
        socket_opts => [
            {port, Port},
            {ip, {0, 0, 0, 0}},  % Listen on all interfaces
            {backlog, 1024},      % Large backlog for high connection rate
            {nodelay, true},      % Disable Nagle's algorithm for lower latency
            {reuseaddr, true}     % Allow address reuse
        ],
        num_acceptors => Acceptors
    },
    
    ProtocolOpts = #{
        env => #{dispatch => Dispatch},
        middlewares => [
            cowboy_router,
            presence_api_cors,      % CORS middleware
            presence_api_auth,      % Authentication middleware
            presence_api_metrics,   % Metrics collection
            cowboy_handler
        ],
        stream_handlers => [cowboy_stream_h],
        max_connections => infinity,  % Let OS handle limits
        request_timeout => 60000,     % 60 second timeout
        idle_timeout => 300000        % 5 minute idle timeout
    },
    
    case cowboy:start_clear(http_listener, TransportOpts, ProtocolOpts) of
        {ok, _} ->
            lager:info("HTTP server started on port ~p", [Port]),
            
            %% Initialize rate limiting
            ok = presence_api_rate_limit:init(),
            
            %% Start the supervisor
            case presence_api_sup:start_link() of
                {ok, Pid} ->
                    lager:info("presence_api application started successfully"),
                    {ok, Pid};
                {error, Reason} = Error ->
                    lager:error("Failed to start presence_api supervisor: ~p", [Reason]),
                    Error
            end;
        {error, Reason} ->
            lager:error("Failed to start HTTP server: ~p", [Reason]),
            {error, {http_server_failed, Reason}}
    end.

%% @doc
%% Stops the presence_api application.
%% @end
-spec stop(State :: term()) -> ok.
stop(_State) ->
    lager:info("Stopping presence_api application"),
    
    %% Stop Cowboy listener
    ok = cowboy:stop_listener(http_listener),
    
    %% Cleanup rate limiting
    ok = presence_api_rate_limit:cleanup(),
    
    ok.

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Get application environment variable with default.
%% @end
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    application:get_env(presence_api, Key, Default).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Compile Cowboy routes.
%%
%% Design Decision: Route organization
%% - /api/v1/* for versioned API endpoints
%% - /ws for WebSocket connections
%% - /health for health checks
%% - Static file serving for potential dashboard
%% @end
-spec compile_routes() -> cowboy_router:dispatch_rules().
compile_routes() ->
    Routes = [
        {'_', [  % Match any host
            %% Health check endpoint (no auth required)
            {"/health", presence_api_health_handler, []},
            
            %% WebSocket endpoint for real-time connections
            %% Trade-off: Single vs multiple WebSocket endpoints
            %% - Chosen: Single endpoint with message-based routing for simplicity
            {"/ws", presence_api_websocket_handler, []},
            
            %% REST API endpoints
            {"/api/v1/presence/:user_id", presence_api_presence_handler, []},
            {"/api/v1/presence", presence_api_presence_list_handler, []},
            {"/api/v1/bulk/presence", presence_api_bulk_handler, []},
            
            %% Static files (if dashboard is included)
            {"/", cowboy_static, {priv_file, presence_api, "static/index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, presence_api, "static"}},
            
            %% Catch-all 404
            {'_', presence_api_not_found_handler, []}
        ]}
    ],
    
    cowboy_router:compile(Routes).