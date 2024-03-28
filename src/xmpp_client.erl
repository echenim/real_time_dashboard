-module(xmpp_client).
-behaviour(gen_server).

%% Necessary imports for XMPP functionality
-include_lib("escalus/include/escalus.hrl").

-export([
    start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).
%% For updating presence status externally if needed
-export([send_presence_update/2]).

-define(SERVER, "localhost").
-define(USERNAME, "user").
-define(PASSWORD, "password").
-define(RESOURCE, "dashboard").

-record(state, {
    client,
    user_presence = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Client} = escalus_client:start(?SERVER, ?USERNAME, ?PASSWORD, ?RESOURCE),
    escalus_client:set_handler(Client, presence, fun handle_presence/3),
    {ok, #state{client = Client}}.

handle_presence(From, Type, State = #state{user_presence = Presence}) ->
    User = escalus_utils:jid_to_bare(From),
    NewPresence =
        case Type of
            available -> Presence#{User => online};
            unavailable -> Presence#{User => offline}
        end,
    gen_server:cast(?MODULE, {update_presence, NewPresence}),
    {ok, State}.

handle_cast({update_presence, NewPresence}, State) ->
    ws_handler:broadcast_presence(NewPresence),
    {noreply, State#state{user_presence = NewPresence}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Utility function to send presence updates (for example, when a user manually changes their status).
send_presence_update(User, Status) ->
    gen_server:cast(?MODULE, {manual_update, User, Status}).
