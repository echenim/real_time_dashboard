%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% presence_store OTP application callback module.
%%%
%%% This application provides a storage abstraction layer that allows
%%% switching between different backends (ETS, Redis, PostgreSQL) without
%%% changing the core application logic.
%%%
%%% Design Decision: Separate storage app vs integrated storage
%%% Trade-off: Extra application vs tight coupling
%%% - Chosen: Separate app for:
%%%   * Clear separation of concerns
%%%   * Ability to deploy without certain backends
%%%   * Easier testing with mock stores
%%%   * Future flexibility for new storage backends
%%% @end
%%%-------------------------------------------------------------------
-module(presence_store_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API for getting configured backend
-export([get_backend/0, get_backend_module/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc
%% Starts the presence_store application.
%%
%% Initializes the configured storage backend.
%% @end
-spec start(StartType, StartArgs) -> {ok, pid()} | {error, Reason} when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Reason :: term().
start(_StartType, _StartArgs) ->
    lager:info("Starting presence_store application"),
    
    %% Get configured backend
    Backend = get_backend(),
    lager:info("Using storage backend: ~p", [Backend]),
    
    %% Initialize the backend
    case init_backend(Backend) of
        ok ->
            %% Start the supervisor
            case presence_store_sup:start_link() of
                {ok, Pid} ->
                    lager:info("presence_store application started successfully"),
                    {ok, Pid};
                {error, Reason} = Error ->
                    lager:error("Failed to start presence_store supervisor: ~p", [Reason]),
                    Error
            end;
        {error, Reason} ->
            lager:error("Failed to initialize backend ~p: ~p", [Backend, Reason]),
            {error, {backend_init_failed, Reason}}
    end.

%% @doc
%% Stops the presence_store application.
%% @end
-spec stop(State :: term()) -> ok.
stop(_State) ->
    lager:info("Stopping presence_store application"),
    
    %% Cleanup backend if needed
    Backend = get_backend(),
    cleanup_backend(Backend),
    
    ok.

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Get the configured storage backend.
%% @end
-spec get_backend() -> atom().
get_backend() ->
    application:get_env(presence_store, backend, ets).

%% @doc
%% Get the module name for the configured backend.
%%
%% This follows the naming convention: presence_store_<backend>
%% @end
-spec get_backend_module() -> module().
get_backend_module() ->
    Backend = get_backend(),
    list_to_atom("presence_store_" ++ atom_to_list(Backend)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Initialize the storage backend.
%%
%% Each backend may have different initialization requirements.
%% @end
-spec init_backend(Backend :: atom()) -> ok | {error, term()}.
init_backend(ets) ->
    %% ETS backend initialization
    %% The actual ETS tables are created in presence_core_app
    %% This is just for any ETS-specific setup
    lager:info("ETS backend ready"),
    ok;

init_backend(redis) ->
    %% Redis backend initialization
    %% Would establish connection pools here
    lager:warning("Redis backend not yet implemented"),
    {error, not_implemented};

init_backend(postgres) ->
    %% PostgreSQL backend initialization
    %% Would establish connection pools and run migrations
    lager:warning("PostgreSQL backend not yet implemented"),
    {error, not_implemented};

init_backend(Backend) ->
    lager:error("Unknown backend: ~p", [Backend]),
    {error, {unknown_backend, Backend}}.

%% @private
%% @doc
%% Cleanup the storage backend on shutdown.
%% @end
-spec cleanup_backend(Backend :: atom()) -> ok.
cleanup_backend(ets) ->
    %% ETS cleanup is handled by presence_core_app
    ok;

cleanup_backend(redis) ->
    %% Would close Redis connections here
    ok;

cleanup_backend(postgres) ->
    %% Would close PostgreSQL connections here
    ok;

cleanup_backend(_Backend) ->
    ok.