-module(ws_handler).
-behaviour(cowboy_websocket).

%% Cowboy WebSocket callback functions
-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).

%% Include necessary headers for WebSocket.
-include_lib("cowboy/include/cowboy_websocket.hrl").

-define(STATE, #{clients => #{}}).

-record(state, {
    % A map to store client PIDs and their associated information
    clients
}).

%% @doc Initialize WebSocket connection
-spec init(map(), cowboy_req:req()) -> {cowboy_websocket, cowboy_req:req(), #state{}}.
init(Req, Opts) ->
    %% Initialize the state with an empty map of clients
    {cowboy_websocket, Req, Opts, #state{clients = #{}}}.

%% @doc Handle incoming WebSocket messages
-spec websocket_handle(cowboy_websocket:frame(), cowboy_req:req(), #state{}) ->
    {ok, cowboy_req:req(), #state{}}.
websocket_handle({text, Msg}, Req, State = #state{clients = Clients}) ->
    %% For simplicity, let's just echo back the received message to all connected clients
    %% In a real application, you'd likely do something more complex here.
    broadcast(Msg, Clients),
    {ok, Req, State}.

%% @doc Handle non-WebSocket messages, like internal Erlang messages or notifications
-spec websocket_info(any(), cowboy_req:req(), #state{}) ->
    {ok, cowboy_req:req(), #state{}}.
websocket_info(_Info, Req, State) ->
    %% Handle other messages, e.g., presence updates from MongooseIM
    {ok, Req, State}.

%% @doc Clean up when WebSocket connection terminates
-spec terminate(cowboy_websocket:close_reason(), cowboy_req:req(), #state{}) ->
    ok.
terminate(_Reason, _Req, _State) ->
    ok.

%% Helper function to broadcast a message to all connected clients
-spec broadcast(binary(), map()) -> ok.
broadcast(Message, Clients) ->
    maps:fold(
        fun(_Pid, _ClientInfo, Acc) ->
            cowboy_websocket:send(_Pid, {text, Message}),
            Acc
        end,
        ok,
        Clients
    ).
