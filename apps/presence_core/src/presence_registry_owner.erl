%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Process that owns the presence registry ETS table.
%%%
%%% This GenServer's primary purpose is to own the ETS table so it
%%% survives if other processes crash. It doesn't actively manage
%%% the table contents - that's done by the registry module.
%%%
%%% Design Decision: Separate owner process for ETS tables
%%% Trade-off: Extra process vs table ownership in supervisor
%%% - Chosen: Separate process for:
%%%   * Clear ownership and lifecycle management
%%%   * Ability to add table monitoring/stats later
%%%   * Decoupling table lifecycle from supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(presence_registry_owner).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% State record
-record(state, {
    tables = [] :: [atom()],         % List of owned table names
    start_time :: erlang:timestamp() % Server start time for uptime tracking
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts the registry owner GenServer.
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initialize the server.
%%
%% Note: We don't create ETS tables here because they're created
%% in the application start callback to ensure they exist before
%% any process tries to use them.
%% @end
-spec init(Args :: term()) -> {ok, #state{}}.
init([]) ->
    %% Log startup
    lager:info("Starting presence registry owner process"),
    
    %% Set process flag to trap exits
    %% This allows us to cleanup properly on shutdown
    process_flag(trap_exit, true),
    
    %% Transfer ownership of existing tables to this process
    %% Trade-off: Transfer vs create new
    %% - Chosen: Transfer to preserve any existing data
    ok = transfer_table_ownership(),
    
    %% Initialize state
    State = #state{
        tables = [presence_registry, presence_metrics],
        start_time = erlang:timestamp()
    },
    
    %% Schedule periodic health checks
    schedule_health_check(),
    
    {ok, State}.

%% @private
%% @doc
%% Handle synchronous calls.
%% @end
-spec handle_call(Request :: term(), From :: {pid(), term()}, 
                  State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}}.

%% Get server statistics
handle_call(get_stats, _From, State) ->
    Stats = #{
        tables => State#state.tables,
        uptime_seconds => timer:now_diff(erlang:timestamp(), State#state.start_time) div 1000000,
        registry_size => ets:info(presence_registry, size),
        metrics_size => ets:info(presence_metrics, size),
        memory => [
            {presence_registry, ets:info(presence_registry, memory)},
            {presence_metrics, ets:info(presence_metrics, memory)}
        ]
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

%% Unknown casts
handle_cast(Msg, State) ->
    lager:warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.

%% @private
%% @doc
%% Handle info messages.
%% @end
-spec handle_info(Info :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.

%% Periodic health check
handle_info(health_check, State) ->
    %% Verify tables still exist and we own them
    lists:foreach(fun(Table) ->
        case ets:info(Table, owner) of
            Self when Self =:= self() ->
                ok;
            undefined ->
                %% Table doesn't exist, recreate it
                lager:error("Table ~p disappeared, recreating", [Table]),
                create_table(Table);
            Other ->
                %% Someone else owns it, this shouldn't happen
                lager:error("Table ~p owned by ~p, not us!", [Table, Other])
        end
    end, State#state.tables),
    
    %% Schedule next check
    schedule_health_check(),
    
    {noreply, State};

%% Handle EXIT signals (trap_exit is enabled)
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:info("Received EXIT from ~p: ~p", [Pid, Reason]),
    {noreply, State};

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
    lager:info("Registry owner terminating: ~p", [Reason]),
    
    %% Note: We don't delete the tables here because they might
    %% be needed for graceful shutdown of other processes
    %% The application stop callback handles final cleanup
    
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
%% Transfer ownership of ETS tables to this process.
%% @end
-spec transfer_table_ownership() -> ok.
transfer_table_ownership() ->
    %% List of tables we should own
    Tables = [presence_registry, presence_metrics],
    
    lists:foreach(fun(Table) ->
        case ets:info(Table, owner) of
            undefined ->
                %% Table doesn't exist, this shouldn't happen
                %% but we'll handle it gracefully
                lager:warning("Table ~p doesn't exist during ownership transfer", [Table]),
                create_table(Table);
            Self when Self =:= self() ->
                %% We already own it
                ok;
            _OtherPid ->
                %% Transfer ownership to us
                %% Note: This requires the table to be public
                try
                    true = ets:give_away(Table, self(), transfer),
                    lager:info("Transferred ownership of table ~p", [Table])
                catch
                    error:badarg ->
                        %% Can't transfer, maybe table is private
                        lager:error("Failed to transfer table ~p", [Table])
                end
        end
    end, Tables),
    
    ok.

%% @private
%% @doc
%% Create an ETS table with appropriate settings.
%% @end
-spec create_table(Table :: atom()) -> ok.
create_table(presence_registry) ->
    ets:new(presence_registry, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),
    ok;
create_table(presence_metrics) ->
    ets:new(presence_metrics, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ok.

%% @private
%% @doc
%% Schedule next health check.
%% @end
-spec schedule_health_check() -> ok.
schedule_health_check() ->
    %% Check every 30 seconds
    %% Trade-off: More frequent checks vs overhead
    %% - Chosen: 30s as reasonable balance
    erlang:send_after(30000, self(), health_check),
    ok.