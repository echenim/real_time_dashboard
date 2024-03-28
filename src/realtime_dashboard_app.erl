-module(realtime_dashboard_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Cowboy handler
-export([index/2]).

start(_StartType, _StartArgs) ->
    %% Define the dispatch rules for Cowboy
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "priv/static/index.html"}},
            {"/ws", ws_handler, []},
            %% You can add more routes here
            {"/static/[...]", cowboy_static, {dir, "priv/static"}}
        ]}
    ]),

    %% Start the Cowboy application
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, 8080}
        ],
        #{env => #{dispatch => Dispatch}}
    ),

    %% Start the supervisor
    realtime_dashboard_sup:start_link().

stop(_State) ->
    ok.

%% HTTP handler to serve the index.html
index(Req, State) ->
    {ok, Req, State}.
