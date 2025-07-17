%%%-------------------------------------------------------------------
%%% @doc
%%% Supervisor for presence tracker processes.
%%% 
%%% This supervisor uses a simple_one_for_one strategy to dynamically
%%% spawn presence_tracker processes for individual users. Each user
%%% gets their own supervised presence tracker process.
%%%
%%% Key design decisions:
%%% - simple_one_for_one strategy for dynamic child creation
%%% - Transient restart strategy - only restart on abnormal termination
%%% - High restart intensity to handle bursts of reconnections
%%% - Child processes are registered via presence_registry
%%%
%%% Trade-offs:
%%% - Simple supervision vs complex hierarchies
%%% - Chose simple approach for clarity and maintainability
%%% - May need partitioned supervisors at extreme scale (>100k users)
%%% @end
%%%-------------------------------------------------------------------
-module(presence_tracker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_tracker/1,
         stop_tracker/1,
         get_tracker_count/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts the supervisor.
%% @returns {ok, Pid} | {error, Reason}
%% @end
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc
%% Starts a new presence tracker for a user.
%% If a tracker already exists for the user, returns error.
%% @param UserId - The unique identifier for the user
%% @returns {ok, Pid} | {error, Reason}
%% @end
-spec start_tracker(binary()) -> {ok, pid()} | {error, term()}.
start_tracker(UserId) when is_binary(UserId) ->
    %% Check if tracker already exists
    case presence_registry:lookup(UserId) of
        {ok, ExistingPid} ->
            case is_process_alive(ExistingPid) of
                true ->
                    ?LOG_WARNING("Tracker already exists for user ~p", [UserId]),
                    {error, {already_started, ExistingPid}};
                false ->
                    start_new_tracker(UserId)
            end;
        _ ->
            start_new_tracker(UserId)
    end.

%% @doc
%% Stops a user's presence tracker gracefully.
%% @param UserId - The unique identifier for the user
%% @returns ok | {error, not_found}
%% @end
-spec stop_tracker(binary()) -> ok | {error, not_found}.
stop_tracker(UserId) when is_binary(UserId) ->
    case presence_registry:lookup(UserId) of
        {ok, Pid} ->
            %% Terminate the child process
            %% The supervisor will handle the actual termination
            ok = supervisor:terminate_child(?SERVER, Pid),
            ?LOG_INFO("Stopped presence tracker for user ~p", [UserId]),
            ok;
        {error, not_found} ->
            ?LOG_WARNING("No tracker found for user ~p", [UserId]),
            {error, not_found}
    end.

%% @doc
%% Gets the count of active presence trackers.
%% @returns Number of active trackers
%% @end
-spec get_tracker_count() -> non_neg_integer().
get_tracker_count() ->
    %% Count all children of this supervisor
    Children = supervisor:which_children(?SERVER),
    length([1 || {_Id, Pid, _Type, _Modules} <- Children, 
                 is_pid(Pid), is_process_alive(Pid)]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc
%% Initializes the supervisor with simple_one_for_one strategy.
%% 
%% Restart strategy explanation:
%% - MaxRestarts = 10: Allow up to 10 restarts
%% - MaxTime = 60: Within 60 seconds
%% - This handles bursts of failures while preventing infinite restart loops
%% 
%% Child spec uses transient restart:
%% - Restart only on abnormal termination
%% - Normal termination (user disconnect) doesn't trigger restart
%% @end
init([]) ->
    ?LOG_INFO("Starting presence tracker supervisor"),
    
    %% Supervisor flags
    %% Trade-off: Restart intensity vs system stability
    %% - Chosen: Moderate intensity to handle reconnection storms
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,    % Max 10 restarts
        period => 60        % per 60 seconds
    },
    
    %% Child specification
    %% Trade-off: Restart strategy (permanent vs transient vs temporary)
    %% - Chosen: Transient - restart on crashes but not normal termination
    ChildSpec = #{
        id => presence_tracker,
        start => {presence_tracker, start_link, []},
        restart => transient,  % Only restart on abnormal exit
        shutdown => 5000,      % 5 second graceful shutdown
        type => worker,
        modules => [presence_tracker]
    },
    
    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Starts a new tracker for the given user
start_new_tracker(UserId) ->
    case supervisor:start_child(?SERVER, [UserId]) of
        {ok, Pid} ->
            ?LOG_INFO("Started presence tracker for user ~p with pid ~p", 
                      [UserId, Pid]),
            {ok, Pid};
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to start tracker for user ~p: ~p", 
                       [UserId, Reason]),
            Error
    end.