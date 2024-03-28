-module(realtime_dashboard_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> {ok, {{one_for_one, 10, 10}, [{cowboy_listener_sup, {cowboy_listener_sup, start_link, [http, 100, [{port, 8080}] , cowboy_protocol, [{env, [{dispatch, Dispatch}] }] ] }, permanent, 5000, supervisor, [cowboy_listener_sup]}]}.
init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ws_handler, []},
            {"/", cowboy_static, {priv_dir, realtime_dashboard, "static/index.html", []}}
        ]}
    ]),

    % Define the Cowboy listener supervisor specification
    CowboySpec =
        {cowboy_listener_sup,
            {cowboy_listener_sup, start_link, [
                http,
                100,
                [{port, 8080}],
                cowboy_clear,
                #{env => #{dispatch => Dispatch}}
            ]},
            permanent, 5000, supervisor, [cowboy_listener_sup]},

    % Define the child specs
    Children = [
        CowboySpec,
        {xmpp_client, {xmpp_client, start_link, []}, permanent, 5000, worker, [xmpp_client]}
        % Add other children here as needed
    ],

    % Set up the supervisor strategy
    {ok, {{one_for_one, 10, 10}, Children}}.
