%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% GenServer for tracking individual user presence.
%%%
%%% Each active user has their own presence_user process that:
%%% - Tracks their online status
%%% - Manages heartbeat timeouts
%%% - Stores user metadata
%%% - Handles graceful disconnection
%%%
%%% Design Decision: GenServer per user
%%% Trade-off: Process overhead vs isolation and concurrency
%%% - Chosen: Process per user for:
%%%   * Fault isolation (one user crash doesn't affect others)  
%%%   * Natural timeout handling (process dies on timeout)
%%%   * No lock contention between users
%%%   * Easy to distribute across nodes
%%% - Downside: ~2KB memory per process
%%%
%%% Lifecycle:
%%% 1. Started by presence_user_sup when user connects
%%% 2. Registers itself in presence_registry
%%% 3. Monitors heartbeats and updates last_seen
%%% 4. Dies on timeout or explicit disconnect
%%% @end
%%%-------------------------------------------------------------------
-module(presence_user).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    heartbeat/1,
    update_metadata/2,
    get_state/1,
    disconnect/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State record
-record(state, {
    user_id :: binary(),
    status = online :: online | away | offline,
    connected_at :: erlang:timestamp(),
    last_heartbeat :: erlang:timestamp(),
    metadata = #{} :: map(),
    heartbeat_ref :: reference() | undefined,
    heartbeat_interval :: pos_integer(),
    offline_timeout :: pos_integer()
}).

%% Default timeout values (milliseconds)
-define(DEFAULT_HEARTBEAT_INTERVAL, 30000).  % 30 seconds
-define(DEFAULT_OFFLINE_TIMEOUT, 90000).     % 90 seconds

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Start a presence process for a user.
%% @end
-spec start_link(UserId :: binary()) -> {ok, pid()} | {error, term()}.
start_link(UserId) when is_binary(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).

%% @doc
%% Send a heartbeat from the user.
%%
%% Resets the timeout timer and updates last_seen timestamp.
%% @end
-spec heartbeat(UserIdOrPid :: binary() | pid()) -> ok | {error, term()}.
heartbeat(UserId) when is_binary(UserId) ->
    case presence_registry:lookup(UserId) of
        {ok, Pid} -> heartbeat(Pid);
        Error -> Error
    end;
heartbeat(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, heartbeat).

%% @doc
%% Update user metadata.
%%
%% Metadata can include things like:
%% - User agent
%% - IP address  
%% - Client version
%% - Custom application data
%% @end
-spec update_metadata(UserIdOrPid :: binary() | pid(), Metadata :: map()) -> 
    ok | {error, term()}.
update_metadata(UserId, Metadata) when is_binary(UserId) ->
    case presence_registry:lookup(UserId) of
        {ok, Pid} -> update_metadata(Pid, Metadata);
        Error -> Error
    end;
update_metadata(Pid, Metadata) when is_pid(Pid), is_map(Metadata) ->
    gen_server:cast(Pid, {update_metadata, Metadata}).

%% @doc
%% Get the current state of a user's presence.
%% @end
-spec get_state(UserIdOrPid :: binary() | pid()) -> 
    {ok, map()} | {error, term()}.
get_state(UserId) when is_binary(UserId) ->
    case presence_registry:lookup(UserId) of
        {ok, Pid} -> get_state(Pid);
        Error -> Error
    end;
get_state(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_state).

%% @doc
%% Explicitly disconnect a user.
%%
%% This will terminate the presence process gracefully.
%% @end
-spec disconnect(UserIdOrPid :: binary() | pid()) -> ok | {error, term()}.
disconnect(UserId) when is_binary(UserId) ->
    case presence_registry:lookup(UserId) of
        {ok, Pid} -> disconnect(Pid);
        Error -> Error
    end;
disconnect(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, disconnect).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initialize the user presence process.
%% @end
-spec init([UserId :: binary()]) -> {ok, #state{}}.
init([UserId]) ->
    %% Log initialization
    lager:info("Starting presence process for user ~s", [UserId]),
    
    %% Get configuration
    HeartbeatInterval = presence_core_app:get_env(heartbeat_interval, ?DEFAULT_HEARTBEAT_INTERVAL),
    OfflineTimeout = presence_core_app:get_env(offline_timeout, ?DEFAULT_OFFLINE_TIMEOUT),
    
    %% Register in the presence registry
    ok = presence_registry:insert(UserId, self()),
    
    %% Emit telemetry event
    telemetry:execute([presence_core, user, online], 
                      #{count => 1}, 
                      #{user_id => UserId}),
    
    %% Initialize state
    Now = erlang:timestamp(),
    State = #state{
        user_id = UserId,
        status = online,
        connected_at = Now,
        last_heartbeat = Now,
        metadata = #{},
        heartbeat_interval = HeartbeatInterval,
        offline_timeout = OfflineTimeout
    },
    
    %% Schedule first timeout check
    HeartbeatRef = schedule_heartbeat_check(State),
    
    {ok, State#state{heartbeat_ref = HeartbeatRef}}.

%% @private
%% @doc
%% Handle synchronous calls.
%% @end
-spec handle_call(Request :: term(), From :: {pid(), term()}, 
                  State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}}.

%% Get current state
handle_call(get_state, _From, State) ->
    StateMap = #{
        user_id => State#state.user_id,
        status => State#state.status,
        connected_at => State#state.connected_at,
        last_heartbeat => State#state.last_heartbeat,
        uptime_seconds => timer:now_diff(erlang:timestamp(), State#state.connected_at) div 1000000,
        metadata => State#state.metadata
    },
    {reply, {ok, StateMap}, State};

%% Unknown calls
handle_call(Request, _From, State) ->
    lager:warning("Unknown call: ~p", [Request]),
    {reply, {error, unknown_request}, State}.

%% @private
%% @doc
%% Handle asynchronous casts.
%% @end
-spec handle_cast(Msg :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} | {stop, Reason :: term(), State :: #state{}}.

%% Heartbeat received
handle_cast(heartbeat, State) ->
    %% Cancel existing timeout
    cancel_heartbeat_check(State#state.heartbeat_ref),
    
    %% Update timestamps
    Now = erlang:timestamp(),
    NewState = State#state{
        last_heartbeat = Now,
        status = online
    },
    
    %% Update registry
    ok = presence_registry:update_last_seen(State#state.user_id),
    
    %% Emit telemetry
    telemetry:execute([presence_core, user, heartbeat], 
                      #{count => 1}, 
                      #{user_id => State#state.user_id}),
    
    %% Schedule next check
    HeartbeatRef = schedule_heartbeat_check(NewState),
    
    {noreply, NewState#state{heartbeat_ref = HeartbeatRef}};

%% Update metadata
handle_cast({update_metadata, NewMetadata}, State) ->
    %% Merge with existing metadata
    %% Trade-off: Replace vs merge
    %% - Chosen: Merge to preserve existing fields
    MergedMetadata = maps:merge(State#state.metadata, NewMetadata),
    NewState = State#state{metadata = MergedMetadata},
    
    lager:debug("Updated metadata for user ~s: ~p", 
                [State#state.user_id, MergedMetadata]),
    
    {noreply, NewState};

%% Explicit disconnect
handle_cast(disconnect, State) ->
    lager:info("User ~s disconnecting", [State#state.user_id]),
    {stop, normal, State};

%% Unknown casts
handle_cast(Msg, State) ->
    lager:warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.

%% @private
%% @doc
%% Handle info messages.
%% @end
-spec handle_info(Info :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} | {stop, Reason :: term(), State :: #state{}}.

%% Heartbeat timeout check
handle_info({heartbeat_timeout, check}, State) ->
    TimeSinceLastHeartbeat = timer:now_diff(erlang:timestamp(), State#state.last_heartbeat),
    
    if
        TimeSinceLastHeartbeat > State#state.offline_timeout * 1000 ->
            %% User timed out
            lager:warning("User ~s timed out after ~p ms", 
                         [State#state.user_id, TimeSinceLastHeartbeat div 1000]),
            {stop, {shutdown, timeout}, State};
        
        TimeSinceLastHeartbeat > State#state.heartbeat_interval * 1000 ->
            %% Mark as away but don't disconnect yet
            %% Trade-off: Immediate disconnect vs grace period
            %% - Chosen: Grace period for network hiccups
            NewState = State#state{status = away},
            HeartbeatRef = schedule_heartbeat_check(NewState),
            {noreply, NewState#state{heartbeat_ref = HeartbeatRef}};
        
        true ->
            %% Still online
            HeartbeatRef = schedule_heartbeat_check(State),
            {noreply, State#state{heartbeat_ref = HeartbeatRef}}
    end;

%% Unknown info
handle_info(Info, State) ->
    lager:warning("Unknown info: ~p", [Info]),
    {noreply, State}.

%% @private
%% @doc
%% Cleanup on termination.
%% @end
-spec terminate(Reason :: term(), State :: #state{}) -> ok.
terminate(Reason, State) ->
    %% Cancel any pending timeouts
    cancel_heartbeat_check(State#state.heartbeat_ref),
    
    %% Remove from registry
    presence_registry:delete(State#state.user_id),
    
    %% Emit offline event
    telemetry:execute([presence_core, user, offline], 
                      #{count => 1}, 
                      #{user_id => State#state.user_id, reason => Reason}),
    
    %% Log based on reason
    case Reason of
        normal ->
            lager:info("User ~s presence process stopped normally", [State#state.user_id]);
        {shutdown, timeout} ->
            lager:info("User ~s timed out", [State#state.user_id]);
        _ ->
            lager:warning("User ~s presence process stopped: ~p", [State#state.user_id, Reason])
    end,
    
    ok.

%% @private
%% @doc
%% Handle code changes.
%% @end
-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) ->
    {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Schedule the next heartbeat timeout check.
%% @end
-spec schedule_heartbeat_check(State :: #state{}) -> reference().
schedule_heartbeat_check(State) ->
    %% Check every heartbeat_interval/3 for responsiveness
    %% Trade-off: Check frequency vs overhead
    %% - Chosen: 1/3 interval for reasonable detection time
    CheckInterval = State#state.heartbeat_interval div 3,
    erlang:send_after(CheckInterval, self(), {heartbeat_timeout, check}).

%% @private
%% @doc
%% Cancel a scheduled heartbeat check.
%% @end
-spec cancel_heartbeat_check(Ref :: reference() | undefined) -> ok.
cancel_heartbeat_check(undefined) -> ok;
cancel_heartbeat_check(Ref) ->
    erlang:cancel_timer(Ref),
    %% Flush any pending messages
    receive
        {heartbeat_timeout, check} -> ok
    after 0 -> ok
    end.