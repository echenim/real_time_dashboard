%%%-------------------------------------------------------------------
%%% @doc
%%% Presence tracker GenServer for managing individual user presence state.
%%% 
%%% This module implements a GenServer that tracks the online/offline status
%%% of a single user. Each user gets their own presence_tracker process,
%%% providing isolation and fault tolerance.
%%%
%%% Key design decisions:
%%% - One GenServer per user for isolation and independent failure handling
%%% - Heartbeat mechanism to detect disconnected clients
%%% - State includes connection metadata for debugging and monitoring
%%% - Automatic cleanup after configurable offline timeout
%%%
%%% Trade-offs:
%%% - Memory usage scales linearly with active users (acceptable for <100k users)
%%% - Process overhead is minimal in BEAM VM
%%% - Isolation prevents cascading failures
%%% @end
%%%-------------------------------------------------------------------
-module(presence_tracker).

-behaviour(gen_server).

%% API
-export([start_link/1,
         update_status/2,
         get_status/1,
         heartbeat/1,
         disconnect/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(HEARTBEAT_TIMEOUT, 30000). % 30 seconds
-define(OFFLINE_TIMEOUT, 90000).   % 90 seconds before cleanup

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts a presence tracker for a specific user.
%% @param UserId - The unique identifier for the user
%% @returns {ok, Pid} | {error, Reason}
%% @end
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(UserId) when is_binary(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).

%% @doc
%% Updates the user's presence status.
%% @param Pid - The process ID of the tracker
%% @param Status - online | offline | away | busy
%% @returns ok
%% @end
-spec update_status(pid(), online | offline | away | busy) -> ok.
update_status(Pid, Status) when is_atom(Status) ->
    gen_server:cast(Pid, {update_status, Status}).

%% @doc
%% Gets the current presence status of a user.
%% @param Pid - The process ID of the tracker
%% @returns {ok, Status} | {error, Reason}
%% @end
-spec get_status(pid()) -> {ok, map()} | {error, term()}.
get_status(Pid) ->
    gen_server:call(Pid, get_status).

%% @doc
%% Updates the heartbeat timestamp, indicating the client is still connected.
%% This resets the heartbeat timer.
%% @param Pid - The process ID of the tracker
%% @returns ok
%% @end
-spec heartbeat(pid()) -> ok.
heartbeat(Pid) ->
    gen_server:cast(Pid, heartbeat).

%% @doc
%% Explicitly disconnects a user and updates their status to offline.
%% @param Pid - The process ID of the tracker
%% @returns ok
%% @end
-spec disconnect(pid()) -> ok.
disconnect(Pid) ->
    gen_server:cast(Pid, disconnect).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initializes the presence tracker state.
%% Sets initial status to online and starts heartbeat timer.
%% @end
init([UserId]) ->
    ?LOG_INFO("Starting presence tracker for user: ~p", [UserId]),
    
    %% Trap exits to handle cleanup properly
    process_flag(trap_exit, true),
    
    %% Register this process with the presence registry
    ok = presence_registry:register(UserId, self()),
    
    State = #{
        user_id => UserId,
        status => online,
        last_seen => erlang:system_time(millisecond),
        connected_at => erlang:system_time(millisecond),
        heartbeat_timer => undefined,
        metadata => #{}
    },
    
    %% Start heartbeat timer
    Timer = erlang:send_after(?HEARTBEAT_TIMEOUT, self(), heartbeat_timeout),
    
    {ok, State#{heartbeat_timer => Timer}}.

%% @private
handle_call(get_status, _From, State) ->
    Status = #{
        user_id => maps:get(user_id, State),
        status => maps:get(status, State),
        last_seen => maps:get(last_seen, State),
        connected_at => maps:get(connected_at, State),
        metadata => maps:get(metadata, State)
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({update_status, NewStatus}, State) ->
    ?LOG_DEBUG("User ~p status update: ~p -> ~p", 
               [maps:get(user_id, State), maps:get(status, State), NewStatus]),
    
    %% Update status and timestamp
    UpdatedState = State#{
        status => NewStatus,
        last_seen => erlang:system_time(millisecond)
    },
    
    %% Notify subscribers about status change
    notify_status_change(UpdatedState),
    
    %% If going offline, start offline timer for cleanup
    UpdatedState2 = case NewStatus of
        offline ->
            %% Cancel heartbeat timer
            cancel_timer(maps:get(heartbeat_timer, State)),
            
            %% Start offline cleanup timer
            OfflineTimer = erlang:send_after(?OFFLINE_TIMEOUT, self(), offline_cleanup),
            UpdatedState#{heartbeat_timer => undefined, offline_timer => OfflineTimer};
        _ ->
            UpdatedState
    end,
    
    {noreply, UpdatedState2};

handle_cast(heartbeat, State) ->
    %% Cancel old timer
    cancel_timer(maps:get(heartbeat_timer, State)),
    
    %% Update last seen
    UpdatedState = State#{
        last_seen => erlang:system_time(millisecond)
    },
    
    %% Start new heartbeat timer
    Timer = erlang:send_after(?HEARTBEAT_TIMEOUT, self(), heartbeat_timeout),
    
    {noreply, UpdatedState#{heartbeat_timer => Timer}};

handle_cast(disconnect, State) ->
    %% Update status to offline and stop
    ?LOG_INFO("User ~p disconnecting", [maps:get(user_id, State)]),
    
    FinalState = State#{
        status => offline,
        last_seen => erlang:system_time(millisecond)
    },
    
    notify_status_change(FinalState),
    
    %% Stop the process gracefully
    {stop, normal, FinalState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(heartbeat_timeout, State) ->
    %% No heartbeat received within timeout
    ?LOG_WARNING("Heartbeat timeout for user ~p", [maps:get(user_id, State)]),
    
    %% Update status to offline
    UpdatedState = State#{
        status => offline,
        last_seen => erlang:system_time(millisecond)
    },
    
    notify_status_change(UpdatedState),
    
    %% Start offline cleanup timer
    OfflineTimer = erlang:send_after(?OFFLINE_TIMEOUT, self(), offline_cleanup),
    
    {noreply, UpdatedState#{heartbeat_timer => undefined, offline_timer => OfflineTimer}};

handle_info(offline_cleanup, State) ->
    %% User has been offline for too long, clean up
    ?LOG_INFO("Cleaning up offline user ~p", [maps:get(user_id, State)]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, State) ->
    ?LOG_INFO("Presence tracker terminating for user ~p: ~p", 
              [maps:get(user_id, State), Reason]),
    
    %% Unregister from registry
    presence_registry:unregister(maps:get(user_id, State)),
    
    %% Cancel any active timers
    cancel_timer(maps:get(heartbeat_timer, State, undefined)),
    cancel_timer(maps:get(offline_timer, State, undefined)),
    
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Cancels a timer if it exists
cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

%% @private
%% @doc Notifies subscribers about status changes
notify_status_change(State) ->
    %% This would integrate with a pub/sub system or event bus
    %% For now, we'll use a simple notification via presence_store
    UserId = maps:get(user_id, State),
    Status = maps:get(status, State),
    LastSeen = maps:get(last_seen, State),
    
    %% Update the presence store
    presence_store:update_presence(UserId, #{
        status => Status,
        last_seen => LastSeen,
        pid => self()
    }),
    
    %% TODO: Implement WebSocket notification to subscribers
    ok.