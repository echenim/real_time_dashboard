%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Registry for active WebSocket connections.
%%%
%%% This module tracks all active WebSocket connections and provides
%%% functionality to broadcast messages to all or specific connections.
%%%
%%% Design Decision: ETS-based registry vs pg2/pg
%%% Trade-off: ETS vs process groups
%%% - Chosen: ETS for:
%%%   * Simple single-node implementation
%%%   * Fast lookups and broadcasts
%%%   * No dependency on pg2/pg
%%% - Future: Can add pg2 for multi-node support
%%% @end
%%%-------------------------------------------------------------------
-module(presence_api_ws_registry).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register/1,
    unregister/1,
    broadcast/1,
    broadcast_to_user/2,
    count_connections/0,
    get_connection_info/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ws_connections).

%% Connection record
-record(connection, {
    pid :: pid(),
    user_id :: binary() | undefined,
    connected_at :: erlang:timestamp(),
    metadata :: map()
}).

%% State record
-record(state, {
    monitor_refs = #{} :: #{pid() => reference()}
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Start the WebSocket registry.
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Register a WebSocket connection.
%% @end
-spec register(Pid :: pid()) -> ok.
register(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {register, Pid}).

%% @doc
%% Unregister a WebSocket connection.
%% @end
-spec unregister(Pid :: pid()) -> ok.
unregister(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {unregister, Pid}).

%% @doc
%% Broadcast a message to all connections.
%%
%% Use sparingly as this sends to ALL connections.
%% @end
-spec broadcast(Message :: term()) -> ok.
broadcast(Message) ->
    gen_server:cast(?SERVER, {broadcast, Message}).

%% @doc
%% Broadcast a message to all connections for a specific user.
%% @end
-spec broadcast_to_user(UserId :: binary(), Message :: term()) -> ok.
broadcast_to_user(UserId, Message) when is_binary(UserId) ->
    gen_server:cast(?SERVER, {broadcast_to_user, UserId, Message}).

%% @doc
%% Count active WebSocket connections.
%% @end
-spec count_connections() -> non_neg_integer().
count_connections() ->
    ets:info(?TABLE, size).

%% @doc
%% Get information about all connections.
%% @end
-spec get_connection_info() -> [map()].
get_connection_info() ->
    ets:foldl(
        fun(#connection{} = Conn, Acc) ->
            Info = #{
                pid => Conn#connection.pid,
                user_id => Conn#connection.user_id,
                connected_at => Conn#connection.connected_at,
                uptime_seconds => timer:now_diff(
                    erlang:timestamp(),
                    Conn#connection.connected_at
                ) div 1000000,
                metadata => Conn#connection.metadata
            },
            [Info | Acc]
        end,
        [],
        ?TABLE
    ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    %% Create ETS table for connections
    %% Trade-off: set vs bag
    %% - Chosen: set with PID as key for uniqueness
    ets:new(?TABLE, [
        named_table,
        set,
        public,  % Allow direct reads from other processes
        {keypos, #connection.pid},
        {read_concurrency, true}
    ]),
    
    lager:info("WebSocket registry started"),
    
    {ok, #state{}}.

%% @private
handle_call({register, Pid}, _From, State) ->
    %% Monitor the process
    MonRef = erlang:monitor(process, Pid),
    
    %% Create connection record
    Connection = #connection{
        pid = Pid,
        connected_at = erlang:timestamp(),
        metadata = #{}
    },
    
    %% Insert into ETS
    true = ets:insert(?TABLE, Connection),
    
    %% Update monitor refs
    NewRefs = maps:put(Pid, MonRef, State#state.monitor_refs),
    
    lager:debug("Registered WebSocket connection ~p", [Pid]),
    
    %% Emit telemetry
    telemetry:execute([presence_api, ws_registry, register], 
                      #{count => 1}, 
                      #{pid => Pid}),
    
    {reply, ok, State#state{monitor_refs = NewRefs}};

handle_call({unregister, Pid}, _From, State) ->
    %% Remove from ETS
    ets:delete(?TABLE, Pid),
    
    %% Demonitor if we were monitoring
    NewRefs = case maps:take(Pid, State#state.monitor_refs) of
        {MonRef, RestRefs} ->
            erlang:demonitor(MonRef, [flush]),
            RestRefs;
        error ->
            State#state.monitor_refs
    end,
    
    lager:debug("Unregistered WebSocket connection ~p", [Pid]),
    
    %% Emit telemetry
    telemetry:execute([presence_api, ws_registry, unregister], 
                      #{count => 1}, 
                      #{pid => Pid}),
    
    {reply, ok, State#state{monitor_refs = NewRefs}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({broadcast, Message}, State) ->
    %% Send to all connections
    %% Trade-off: ets:foldl vs ets:tab2list
    %% - Chosen: foldl to avoid building intermediate list
    Count = ets:foldl(
        fun(#connection{pid = Pid}, Acc) ->
            Pid ! Message,
            Acc + 1
        end,
        0,
        ?TABLE
    ),
    
    lager:debug("Broadcast message to ~p connections", [Count]),
    
    %% Emit telemetry
    telemetry:execute([presence_api, ws_registry, broadcast], 
                      #{count => Count}, 
                      #{}),
    
    {noreply, State};

handle_cast({broadcast_to_user, UserId, Message}, State) ->
    %% Find connections for user
    %% Trade-off: Index by user_id vs scan
    %% - Chosen: Scan for now, can add index if needed
    Connections = ets:match_object(?TABLE, #connection{user_id = UserId, _ = '_'}),
    
    %% Send to each connection
    Count = lists:foldl(
        fun(#connection{pid = Pid}, Acc) ->
            Pid ! Message,
            Acc + 1
        end,
        0,
        Connections
    ),
    
    lager:debug("Broadcast message to ~p connections for user ~s", [Count, UserId]),
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', MonRef, process, Pid, _Reason}, State) ->
    %% Process died, clean up
    %% Remove from ETS
    ets:delete(?TABLE, Pid),
    
    %% Remove monitor ref
    NewRefs = maps:remove(Pid, State#state.monitor_refs),
    
    lager:debug("WebSocket connection ~p died", [Pid]),
    
    {noreply, State#state{monitor_refs = NewRefs}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.