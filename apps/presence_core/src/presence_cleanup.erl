%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Cleanup process for handling timed-out users and maintenance tasks.
%%%
%%% This GenServer periodically:
%%% - Scans for dead processes in the registry
%%% - Cleans up orphaned entries
%%% - Collects and publishes metrics
%%% - Performs other maintenance tasks
%%%
%%% Design Decision: Separate cleanup process vs inline cleanup
%%% Trade-off: Extra process vs distributed cleanup logic
%%% - Chosen: Separate process for:
%%%   * Centralized maintenance logic
%%%   * Predictable cleanup intervals
%%%   * Ability to add more maintenance tasks
%%%   * Better monitoring and debugging
%%% @end
%%%-------------------------------------------------------------------
-module(presence_cleanup).

-behaviour(gen_server).

%% API
-export([start_link/0, force_cleanup/0, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_CLEANUP_INTERVAL, 60000).  % 1 minute

%% State record
-record(state, {
    cleanup_interval :: pos_integer(),
    last_cleanup :: erlang:timestamp(),
    cleanup_count = 0 :: non_neg_integer(),
    metrics = #{} :: map()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts the cleanup GenServer.
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Force an immediate cleanup cycle.
%%
%% Useful for testing or administrative purposes.
%% @end
-spec force_cleanup() -> {ok, map()} | {error, term()}.
force_cleanup() ->
    gen_server:call(?SERVER, force_cleanup).

%% @doc
%% Get cleanup statistics.
%% @end
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initialize the cleanup server.
%% @end
-spec init(Args :: term()) -> {ok, #state{}}.
init([]) ->
    lager:info("Starting presence cleanup process"),
    
    %% Get configuration
    CleanupInterval = presence_core_app:get_env(
        registry_cleanup_interval, 
        ?DEFAULT_CLEANUP_INTERVAL
    ),
    
    %% Initialize state
    State = #state{
        cleanup_interval = CleanupInterval,
        last_cleanup = erlang:timestamp()
    },
    
    %% Schedule first cleanup
    schedule_cleanup(State),
    
    %% Initialize metrics in ETS
    init_metrics(),
    
    {ok, State}.

%% @private
%% @doc
%% Handle synchronous calls.
%% @end
-spec handle_call(Request :: term(), From :: {pid(), term()}, 
                  State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}}.

%% Force immediate cleanup
handle_call(force_cleanup, _From, State) ->
    lager:info("Forcing cleanup cycle"),
    {CleanupResult, NewState} = perform_cleanup(State),
    {reply, {ok, CleanupResult}, NewState};

%% Get statistics
handle_call(get_stats, _From, State) ->
    Stats = #{
        cleanup_interval => State#state.cleanup_interval,
        last_cleanup => State#state.last_cleanup,
        cleanup_count => State#state.cleanup_count,
        time_since_last_cleanup => timer:now_diff(
            erlang:timestamp(), 
            State#state.last_cleanup
        ) div 1000000,
        current_metrics => State#state.metrics
    },
    {reply, {ok, Stats}, State};

%% Unknown calls
handle_call(Request, _From, State) ->
    lager:warning("Unknown call: ~p", [Request]),
    {reply, {error, unknown_request}, State}.

%% @private
%% @doc
%% Handle asynchronous casts.
%% @end
-spec handle_cast(Msg :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.

handle_cast(Msg, State) ->
    lager:warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.

%% @private
%% @doc
%% Handle info messages.
%% @end
-spec handle_info(Info :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.

%% Scheduled cleanup
handle_info(cleanup, State) ->
    {_Result, NewState} = perform_cleanup(State),
    
    %% Schedule next cleanup
    schedule_cleanup(NewState),
    
    {noreply, NewState};

%% Unknown info
handle_info(Info, State) ->
    lager:warning("Unknown info: ~p", [Info]),
    {noreply, State}.

%% @private
%% @doc
%% Cleanup on termination.
%% @end
-spec terminate(Reason :: term(), State :: #state{}) -> ok.
terminate(Reason, _State) ->
    lager:info("Cleanup process terminating: ~p", [Reason]),
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
%% Schedule the next cleanup cycle.
%% @end
-spec schedule_cleanup(State :: #state{}) -> ok.
schedule_cleanup(State) ->
    erlang:send_after(State#state.cleanup_interval, self(), cleanup),
    ok.

%% @private
%% @doc
%% Perform cleanup tasks.
%%
%% Returns cleanup results and updated state.
%% @end
-spec perform_cleanup(State :: #state{}) -> {map(), #state{}}.
perform_cleanup(State) ->
    StartTime = erlang:timestamp(),
    
    %% Task 1: Clean dead processes from registry
    DeadProcesses = clean_dead_processes(),
    
    %% Task 2: Collect metrics
    Metrics = collect_metrics(),
    
    %% Task 3: Update metrics in ETS
    update_metrics(Metrics),
    
    %% Task 4: Check for anomalies
    Anomalies = check_anomalies(Metrics),
    
    %% Calculate cleanup duration
    Duration = timer:now_diff(erlang:timestamp(), StartTime) div 1000,
    
    %% Prepare result
    Result = #{
        dead_processes_cleaned => DeadProcesses,
        metrics => Metrics,
        anomalies => Anomalies,
        duration_ms => Duration
    },
    
    %% Update state
    NewState = State#state{
        last_cleanup = erlang:timestamp(),
        cleanup_count = State#state.cleanup_count + 1,
        metrics = Metrics
    },
    
    %% Log summary
    lager:info("Cleanup cycle completed: ~p dead processes removed, ~p ms", 
               [DeadProcesses, Duration]),
    
    %% Emit telemetry
    telemetry:execute([presence_core, cleanup, cycle], 
                      #{dead_processes => DeadProcesses, duration_ms => Duration}, 
                      #{}),
    
    {Result, NewState}.

%% @private
%% @doc
%% Clean dead processes from the registry.
%%
%% Trade-off: Full scan vs incremental cleanup
%% - Chosen: Full scan for simplicity and correctness
%% - Could optimize with incremental approach if needed
%% @end
-spec clean_dead_processes() -> non_neg_integer().
clean_dead_processes() ->
    %% Get all registry entries
    AllEntries = ets:tab2list(presence_registry),
    
    %% Check each entry and remove dead ones
    DeadCount = lists:foldl(
        fun({UserId, Entry}, Count) ->
            Pid = get_pid_from_entry(Entry),
            case is_process_alive(Pid) of
                true -> 
                    Count;
                false ->
                    lager:debug("Removing dead process for user ~s", [UserId]),
                    presence_registry:delete(UserId),
                    Count + 1
            end
        end,
        0,
        AllEntries
    ),
    
    DeadCount.

%% @private
%% @doc
%% Extract PID from registry entry (handles different entry formats).
%% @end
-spec get_pid_from_entry(Entry :: term()) -> pid() | undefined.
get_pid_from_entry({registry_entry, _UserId, Pid, _Node, _Started, _LastSeen, _Meta}) ->
    Pid;
get_pid_from_entry(Pid) when is_pid(Pid) ->
    Pid;
get_pid_from_entry(_) ->
    undefined.

%% @private
%% @doc
%% Collect system metrics.
%% @end
-spec collect_metrics() -> map().
collect_metrics() ->
    %% Basic metrics
    ActiveUsers = presence_registry:count(),
    
    %% Memory metrics
    MemoryInfo = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    
    %% Registry table metrics
    RegistryInfo = case ets:info(presence_registry) of
        undefined -> #{};
        Info -> maps:from_list(Info)
    end,
    
    %% Build metrics map
    #{
        active_users => ActiveUsers,
        process_count => ProcessCount,
        memory_total => proplists:get_value(total, MemoryInfo, 0),
        memory_processes => proplists:get_value(processes, MemoryInfo, 0),
        memory_ets => proplists:get_value(ets, MemoryInfo, 0),
        registry_memory => maps:get(memory, RegistryInfo, 0),
        timestamp => erlang:timestamp()
    }.

%% @private
%% @doc
%% Update metrics in ETS table.
%% @end
-spec update_metrics(Metrics :: map()) -> ok.
update_metrics(Metrics) ->
    %% Store current metrics
    ets:insert(presence_metrics, {current_metrics, Metrics}),
    
    %% Update historical max values
    lists:foreach(
        fun({Key, Value}) when is_number(Value) ->
            case ets:lookup(presence_metrics, {max, Key}) of
                [] ->
                    ets:insert(presence_metrics, {{max, Key}, Value});
                [{_, OldMax}] when Value > OldMax ->
                    ets:insert(presence_metrics, {{max, Key}, Value});
                _ ->
                    ok
            end
        end,
        maps:to_list(Metrics)
    ),
    
    ok.

%% @private
%% @doc
%% Check for system anomalies.
%%
%% Returns a list of detected anomalies.
%% @end
-spec check_anomalies(Metrics :: map()) -> [term()].
check_anomalies(Metrics) ->
    Anomalies = [],
    
    %% Check 1: High process count relative to users
    %% Trade-off: Fixed threshold vs dynamic
    %% - Chosen: Simple ratio for clarity
    ActiveUsers = maps:get(active_users, Metrics, 0),
    ProcessCount = maps:get(process_count, Metrics, 0),
    Anomalies1 = case ActiveUsers > 0 of
        true when ProcessCount / ActiveUsers > 10 ->
            [{high_process_ratio, ProcessCount / ActiveUsers} | Anomalies];
        _ ->
            Anomalies
    end,
    
    %% Check 2: High memory usage
    MemoryTotal = maps:get(memory_total, Metrics, 0),
    Anomalies2 = case MemoryTotal > 1024 * 1024 * 1024 of  % 1GB
        true ->
            [{high_memory, MemoryTotal} | Anomalies1];
        false ->
            Anomalies1
    end,
    
    %% Log anomalies
    case Anomalies2 of
        [] -> ok;
        _ -> lager:warning("Anomalies detected: ~p", [Anomalies2])
    end,
    
    Anomalies2.

%% @private
%% @doc
%% Initialize metrics storage.
%% @end
-spec init_metrics() -> ok.
init_metrics() ->
    %% Initialize with empty metrics
    ets:insert(presence_metrics, {current_metrics, #{}}),
    ok.