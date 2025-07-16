%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team  
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Dynamic supervisor for user presence processes.
%%%
%%% This supervisor manages individual user presence GenServers.
%%% Each connected user gets their own supervised process.
%%%
%%% Design Decision: One process per user
%%% Trade-off: Process per user vs shared state
%%% - Chosen: Process per user for:
%%%   * Fault isolation (one user crash doesn't affect others)
%%%   * Natural concurrency (no lock contention)
%%%   * Easy timeout handling (process dies on timeout)
%%% - Downside: More memory usage (~2KB per process)
%%%
%%% Design Decision: Dynamic supervisor vs simple_one_for_one
%%% - Chosen: DynamicSupervisor (OTP 24+) for better API and flexibility
%%% @end
%%%-------------------------------------------------------------------
-module(presence_user_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_user/1, stop_user/1, count_users/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts the dynamic supervisor for user processes.
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc
%% Start a new user presence process.
%%
%% Creates a new supervised process for tracking a user's presence.
%% If a process already exists for this user, returns the existing PID.
%%
%% Trade-off: Return existing vs error on duplicate
%% - Chosen: Return existing for idempotency
%% @end
-spec start_user(UserId :: binary()) -> {ok, pid()} | {error, term()}.
start_user(UserId) when is_binary(UserId) ->
    %% Check if user already has a process
    case presence_registry:lookup(UserId) of
        {ok, Pid} ->
            %% Process already exists, return it
            %% This makes the operation idempotent
            lager:debug("User ~s already has presence process ~p", [UserId, Pid]),
            {ok, Pid};
        {error, not_found} ->
            %% Start new child process
            ChildSpec = #{
                id => UserId,  % Use UserId as child ID for easy identification
                start => {presence_user, start_link, [UserId]},
                restart => transient,  % Don't restart if normal exit
                shutdown => 5000,      % 5 seconds to cleanup
                type => worker,
                modules => [presence_user]
            },
            
            case supervisor:start_child(?SERVER, ChildSpec) of
                {ok, Pid} = Success ->
                    lager:info("Started presence process for user ~s: ~p", [UserId, Pid]),
                    telemetry:execute([presence_core, user_sup, start], 
                                    #{count => 1}, 
                                    #{user_id => UserId}),
                    Success;
                {error, {already_started, Pid}} ->
                    %% Race condition: another process started it first
                    lager:debug("Race condition: user ~s already started", [UserId]),
                    {ok, Pid};
                {error, Reason} = Error ->
                    lager:error("Failed to start presence for user ~s: ~p", [UserId, Reason]),
                    Error
            end
    end.

%% @doc
%% Stop a user presence process.
%%
%% Gracefully terminates the user's presence process.
%% @end
-spec stop_user(UserId :: binary()) -> ok | {error, not_found}.
stop_user(UserId) when is_binary(UserId) ->
    case supervisor:terminate_child(?SERVER, UserId) of
        ok ->
            %% Also delete from supervisor to allow restart with same ID
            supervisor:delete_child(?SERVER, UserId),
            lager:info("Stopped presence process for user ~s", [UserId]),
            telemetry:execute([presence_core, user_sup, stop], 
                            #{count => 1}, 
                            #{user_id => UserId}),
            ok;
        {error, not_found} ->
            lager:warning("No presence process found for user ~s", [UserId]),
            {error, not_found}
    end.

%% @doc
%% Count the number of active user processes.
%%
%% Useful for monitoring and capacity planning.
%% @end
-spec count_users() -> non_neg_integer().
count_users() ->
    %% Count all children of this supervisor
    Children = supervisor:which_children(?SERVER),
    length(Children).

%%%===================================================================
%%% Supervisor callbacks  
%%%===================================================================

%% @private
%% @doc
%% Initialize the supervisor.
%%
%% Using a dynamic supervisor pattern for flexible child management.
%% @end
-spec init(Args :: term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Supervisor flags for dynamic supervisor
    %% Trade-off: Restart intensity
    %% - Too low: legitimate users might fail to reconnect
    %% - Too high: runaway processes could consume resources
    %% - Chosen: 100 restarts in 60 seconds (high for user-facing system)
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,  
        period => 60
    },
    
    %% No static children for dynamic supervisor
    Children = [],
    
    {ok, {SupFlags, Children}}.