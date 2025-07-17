%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% WebSocket handler for real-time presence updates.
%%%
%%% This handler manages WebSocket connections for clients to receive
%%% real-time presence updates. It supports:
%%% - Authentication via JWT token
%%% - Heartbeat/ping-pong for connection health
%%% - Subscription to specific users or all users
%%% - Real-time presence change notifications
%%%
%%% Design Decision: Cowboy WebSocket behavior
%%% Trade-off: Raw WebSocket vs Socket.IO/Phoenix Channels
%%% - Chosen: Raw WebSocket for:
%%%   * Lower overhead
%%%   * Better control over protocol
%%%   * No client library dependencies
%%% - Downside: Need to implement features like reconnection ourselves
%%%
%%% Protocol Design:
%%% Client -> Server: JSON messages with {type, payload}
%%% Server -> Client: JSON messages with {type, data, timestamp}
%%% @end
%%%-------------------------------------------------------------------
-module(presence_api_websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% State record for WebSocket connection
-record(state, {
    user_id :: binary() | undefined,
    authenticated = false :: boolean(),
    subscriptions = [] :: [binary()],  % List of user IDs to watch
    last_ping :: erlang:timestamp(),
    metadata = #{} :: map()
}).

%%%===================================================================
%%% Cowboy WebSocket callbacks
%%%===================================================================

%% @doc
%% Initialize the WebSocket connection.
%%
%% Upgrade HTTP connection to WebSocket.
%% @end
-spec init(Req, Opts) -> {cowboy_websocket, Req, State} when
    Req :: cowboy_req:req(),
    Opts :: any(),
    State :: #state{}.
init(Req, _Opts) ->
    %% Check if WebSocket upgrade is requested
    case cowboy_req:parse_header(<<"upgrade">>, Req) of
        [<<"websocket">>] ->
            %% Get client IP for rate limiting
            {{IP, _Port}, _} = cowboy_req:peer(Req),
            
            %% Check rate limit
            case presence_api_rate_limit:check_ws_connection(IP) of
                ok ->
                    %% Extract JWT from query params or Authorization header
                    _Token = extract_token(Req),  % Will be used in auth message
                    
                    %% Initial state
                    State = #state{
                        last_ping = erlang:timestamp(),
                        metadata = #{
                            ip => IP,
                            connected_at => erlang:timestamp(),
                            user_agent => cowboy_req:header(<<"user-agent">>, Req, <<>>)
                        }
                    },
                    
                    %% Upgrade to WebSocket
                    %% Trade-off: Compression settings
                    %% - Chosen: Enable compression for bandwidth savings
                    {cowboy_websocket, Req, State, #{
                        compress => true,
                        idle_timeout => 60000,  % 1 minute idle timeout
                        max_frame_size => 65536 % 64KB max frame
                    }};
                {error, rate_limited} ->
                    %% Return 429 Too Many Requests
                    Req2 = cowboy_req:reply(429, #{
                        <<"content-type">> => <<"text/plain">>
                    }, <<"Rate limit exceeded">>, Req),
                    {ok, Req2, undefined}
            end;
        _ ->
            %% Not a WebSocket upgrade request
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"text/plain">>
            }, <<"WebSocket upgrade required">>, Req),
            {ok, Req2, undefined}
    end.

%% @doc
%% WebSocket connection established.
%%
%% Send initial connection acknowledgment.
%% @end
-spec websocket_init(State :: #state{}) -> {ok, State :: #state{}}.
websocket_init(State) ->
    %% Register connection in WebSocket registry
    ok = presence_api_ws_registry:register(self()),
    
    %% Send connection acknowledgment
    Msg = jsx:encode(#{
        type => <<"connected">>,
        data => #{
            server_time => erlang:system_time(millisecond),
            protocol_version => <<"1.0">>
        }
    }),
    
    %% Emit telemetry
    telemetry:execute([presence_api, ws, connect], #{count => 1}, #{}),
    
    %% Schedule ping timer
    erlang:send_after(30000, self(), ping),
    
    {reply, {text, Msg}, State}.

%% @doc
%% Handle incoming WebSocket frames.
%%
%% Processes client messages and returns appropriate responses.
%% @end
-spec websocket_handle(Frame, State) -> Result when
    Frame :: {text | binary, binary()} | ping | pong,
    State :: #state{},
    Result :: {ok, State} | {reply, Reply, State} | {stop, State},
    Reply :: {text | binary | ping | pong, binary()}.

%% Text frame with JSON message
websocket_handle({text, Data}, State) ->
    try
        %% Parse JSON message
        Msg = jsx:decode(Data, [return_maps]),
        Type = maps:get(<<"type">>, Msg, undefined),
        Payload = maps:get(<<"payload">>, Msg, #{}),
        
        %% Route based on message type
        handle_client_message(Type, Payload, State)
    catch
        error:badarg ->
            %% Invalid JSON
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                data => #{
                    code => <<"INVALID_JSON">>,
                    message => <<"Invalid JSON format">>
                }
            }),
            {reply, {text, ErrorMsg}, State}
    end;

%% Binary frames not supported
websocket_handle({binary, _}, State) ->
    ErrorMsg = jsx:encode(#{
        type => <<"error">>,
        data => #{
            code => <<"BINARY_NOT_SUPPORTED">>,
            message => <<"Binary frames not supported">>
        }
    }),
    {reply, {text, ErrorMsg}, State};

%% Ping frame - respond with pong
websocket_handle(ping, State) ->
    {reply, pong, State};

%% Pong frame - update last ping time
websocket_handle(pong, State) ->
    {ok, State#state{last_ping = erlang:timestamp()}};

%% Unknown frame
websocket_handle(_Frame, State) ->
    {ok, State}.

%% @doc
%% Handle Erlang messages.
%%
%% Processes internal messages and notifications.
%% @end
-spec websocket_info(Info, State) -> Result when
    Info :: any(),
    State :: #state{},
    Result :: {ok, State} | {reply, Reply, State} | {stop, State},
    Reply :: {text | binary | ping | pong, binary()}.

%% Scheduled ping
websocket_info(ping, State) ->
    %% Send ping to client
    erlang:send_after(30000, self(), ping),
    {reply, ping, State};

%% Presence update notification
websocket_info({presence_update, UserId, Status}, State) ->
    %% Check if client is subscribed to this user
    case lists:member(UserId, State#state.subscriptions) orelse 
         State#state.subscriptions =:= all of
        true ->
            %% Send update to client
            Msg = jsx:encode(#{
                type => <<"presence_update">>,
                data => #{
                    user_id => UserId,
                    status => Status,
                    timestamp => erlang:system_time(millisecond)
                }
            }),
            {reply, {text, Msg}, State};
        false ->
            %% Not subscribed, ignore
            {ok, State}
    end;

%% Unknown message
websocket_info(_Info, State) ->
    {ok, State}.

%% @doc
%% Clean up on connection termination.
%% @end
-spec terminate(Reason, Req, State) -> ok when
    Reason :: any(),
    Req :: cowboy_req:req(),
    State :: #state{}.
terminate(_Reason, _Req, State) ->
    %% Unregister from WebSocket registry
    ok = presence_api_ws_registry:unregister(self()),
    
    %% Disconnect user if authenticated
    case State#state.authenticated of
        true when State#state.user_id =/= undefined ->
            presence_user:disconnect(State#state.user_id);
        _ ->
            ok
    end,
    
    %% Emit telemetry
    telemetry:execute([presence_api, ws, disconnect], #{count => 1}, #{}),
    
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Extract JWT token from request.
%% @end
-spec extract_token(Req :: cowboy_req:req()) -> binary() | undefined.
extract_token(Req) ->
    %% Try query parameter first
    QsVals = cowboy_req:parse_qs(Req),
    case lists:keyfind(<<"token">>, 1, QsVals) of
        {_, Token} -> Token;
        false ->
            %% Try Authorization header
            case cowboy_req:header(<<"authorization">>, Req) of
                <<"Bearer ", Token/binary>> -> Token;
                _ -> undefined
            end
    end.

%% @private
%% @doc
%% Handle client message based on type.
%% @end
-spec handle_client_message(Type :: binary(), Payload :: map(), State :: #state{}) ->
    {reply, {text, binary()}, #state{}} | {ok, #state{}} | {stop, #state{}}.

%% Authentication message
handle_client_message(<<"auth">>, Payload, State) ->
    Token = maps:get(<<"token">>, Payload, undefined),
    
    case presence_api_auth:verify_token(Token) of
        {ok, Claims} ->
            UserId = maps:get(<<"sub">>, Claims),
            
            %% Start presence tracking for user
            case presence_user_sup:start_user(UserId) of
                {ok, _Pid} ->
                    %% Update state
                    NewState = State#state{
                        user_id = UserId,
                        authenticated = true
                    },
                    
                    %% Send success response
                    Msg = jsx:encode(#{
                        type => <<"auth_success">>,
                        data => #{
                            user_id => UserId,
                            expires_at => maps:get(<<"exp">>, Claims, null)
                        }
                    }),
                    
                    {reply, {text, Msg}, NewState};
                {error, Reason} ->
                    ErrorMsg = jsx:encode(#{
                        type => <<"error">>,
                        data => #{
                            code => <<"AUTH_FAILED">>,
                            message => iolist_to_binary(io_lib:format("~p", [Reason]))
                        }
                    }),
                    {reply, {text, ErrorMsg}, State}
            end;
        {error, _} ->
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                data => #{
                    code => <<"INVALID_TOKEN">>,
                    message => <<"Invalid or expired token">>
                }
            }),
            {reply, {text, ErrorMsg}, State}
    end;

%% Heartbeat message
handle_client_message(<<"heartbeat">>, _Payload, State) when State#state.authenticated ->
    %% Update user heartbeat
    presence_user:heartbeat(State#state.user_id),
    
    %% Send acknowledgment
    Msg = jsx:encode(#{
        type => <<"heartbeat_ack">>,
        data => #{
            timestamp => erlang:system_time(millisecond)
        }
    }),
    
    {reply, {text, Msg}, State};

%% Subscribe to user updates
handle_client_message(<<"subscribe">>, Payload, State) when State#state.authenticated ->
    UserIds = maps:get(<<"user_ids">>, Payload, []),
    
    %% Validate user IDs
    case validate_user_ids(UserIds) of
        ok ->
            %% Update subscriptions
            NewState = State#state{subscriptions = UserIds},
            
            %% Send current status of subscribed users
            Users = get_users_status(UserIds),
            
            Msg = jsx:encode(#{
                type => <<"subscribed">>,
                data => #{
                    users => Users
                }
            }),
            
            {reply, {text, Msg}, NewState};
        {error, invalid_user_ids} ->
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                data => #{
                    code => <<"INVALID_USER_IDS">>,
                    message => <<"Invalid user IDs provided">>
                }
            }),
            {reply, {text, ErrorMsg}, State}
    end;

%% Subscribe to all updates
handle_client_message(<<"subscribe_all">>, _Payload, State) when State#state.authenticated ->
    %% Check permission (might want to restrict this)
    NewState = State#state{subscriptions = all},
    
    Msg = jsx:encode(#{
        type => <<"subscribed_all">>,
        data => #{
            message => <<"Subscribed to all user updates">>
        }
    }),
    
    {reply, {text, Msg}, NewState};

%% Disconnect
handle_client_message(<<"disconnect">>, _Payload, _State) ->
    {stop, normal};

%% Unauthenticated request
handle_client_message(_Type, _Payload, State) when not State#state.authenticated ->
    ErrorMsg = jsx:encode(#{
        type => <<"error">>,
        data => #{
            code => <<"UNAUTHENTICATED">>,
            message => <<"Authentication required">>
        }
    }),
    {reply, {text, ErrorMsg}, State};

%% Unknown message type
handle_client_message(_Type, _Payload, State) ->
    ErrorMsg = jsx:encode(#{
        type => <<"error">>,
        data => #{
            code => <<"UNKNOWN_MESSAGE_TYPE">>,
            message => <<"Unknown message type">>
        }
    }),
    {reply, {text, ErrorMsg}, State}.

%% @private
%% @doc
%% Validate user IDs list.
%% @end
-spec validate_user_ids(UserIds :: term()) -> ok | {error, invalid_user_ids}.
validate_user_ids(UserIds) when is_list(UserIds) ->
    case lists:all(fun is_binary/1, UserIds) of
        true -> ok;
        false -> {error, invalid_user_ids}
    end;
validate_user_ids(_) ->
    {error, invalid_user_ids}.

%% @private
%% @doc
%% Get current status of users.
%% @end
-spec get_users_status(UserIds :: [binary()]) -> [map()].
get_users_status(UserIds) ->
    lists:filtermap(
        fun(UserId) ->
            case presence_user:get_state(UserId) of
                {ok, State} ->
                    {true, #{
                        user_id => UserId,
                        status => maps:get(status, State, offline),
                        last_seen => maps:get(last_heartbeat, State, null)
                    }};
                {error, not_found} ->
                    {true, #{
                        user_id => UserId,
                        status => offline,
                        last_seen => null
                    }}
            end
        end,
        UserIds
    ).