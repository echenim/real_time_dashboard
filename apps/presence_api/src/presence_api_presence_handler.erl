%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% REST API handler for individual user presence operations.
%%%
%%% Handles:
%%% - GET /api/v1/presence/:user_id - Get user presence
%%% - PUT /api/v1/presence/:user_id - Update user presence
%%% - DELETE /api/v1/presence/:user_id - Remove user presence
%%%
%%% Design Decision: RESTful API design
%%% Trade-off: REST vs GraphQL vs RPC
%%% - Chosen: REST for:
%%%   * Simple, well-understood semantics
%%%   * Easy caching
%%%   * Wide tooling support
%%% @end
%%%-------------------------------------------------------------------
-module(presence_api_presence_handler).

-export([init/2]).

%% Cowboy REST callbacks
-export([
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    delete_resource/2
]).

%% Content handlers
-export([
    get_presence/2,
    update_presence/2
]).

%% State record
-record(state, {
    user_id :: binary() | undefined,
    auth_claims :: map(),
    exists = false :: boolean()
}).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%% @doc
%% Initialize the handler.
%% @end
init(Req, Opts) ->
    %% Extract user ID from path
    UserId = cowboy_req:binding(user_id, Req),
    
    %% Get auth claims from middleware
    AuthClaims = maps:get(auth_claims, Opts, #{}),
    
    %% Check rate limit
    {{IP, _Port}, _} = cowboy_req:peer(Req),
    case presence_api_rate_limit:check_request(IP, <<"/api/v1/presence">>) of
        ok ->
            State = #state{
                user_id = UserId,
                auth_claims = AuthClaims
            },
            {cowboy_rest, Req, State};
        {error, rate_limited} ->
            %% Return 429 Too Many Requests
            Req2 = cowboy_req:reply(429, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"rate_limited">>}), Req),
            {ok, Req2, undefined}
    end.

%% @doc
%% Allowed HTTP methods.
%% @end
allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>],
    {Methods, Req, State}.

%% @doc
%% Content types provided for GET requests.
%% @end
content_types_provided(Req, State) ->
    Providers = [
        {<<"application/json">>, get_presence}
    ],
    {Providers, Req, State}.

%% @doc
%% Content types accepted for PUT requests.
%% @end
content_types_accepted(Req, State) ->
    Acceptors = [
        {<<"application/json">>, update_presence}
    ],
    {Acceptors, Req, State}.

%% @doc
%% Check if resource exists.
%%
%% Sets exists flag in state for GET/DELETE operations.
%% @end
resource_exists(Req, #state{user_id = UserId} = State) ->
    case presence_user:get_state(UserId) of
        {ok, _} ->
            {true, Req, State#state{exists = true}};
        {error, not_found} ->
            {false, Req, State#state{exists = false}}
    end.

%% @doc
%% Delete user presence.
%% @end
delete_resource(Req, #state{user_id = UserId} = State) ->
    %% Disconnect user
    ok = presence_user:disconnect(UserId),
    
    %% Emit telemetry
    telemetry:execute([presence_api, rest, delete], 
                      #{count => 1}, 
                      #{user_id => UserId}),
    
    {true, Req, State}.

%%%===================================================================
%%% Content handlers
%%%===================================================================

%% @doc
%% Handle GET request - retrieve user presence.
%% @end
get_presence(Req, #state{user_id = UserId} = State) ->
    case presence_user:get_state(UserId) of
        {ok, UserState} ->
            %% Get additional data from store
            StoreData = case presence_store:get_user(UserId) of
                {ok, Data} -> Data;
                {error, not_found} -> #{}
            end,
            
            %% Combine presence state with store data
            %% Trade-off: Separate vs combined data
            %% - Chosen: Combined for single API call convenience
            Response = #{
                user_id => UserId,
                status => maps:get(status, UserState, offline),
                connected_at => maps:get(connected_at, UserState, null),
                last_heartbeat => maps:get(last_heartbeat, UserState, null),
                uptime_seconds => maps:get(uptime_seconds, UserState, 0),
                metadata => maps:get(metadata, UserState, #{}),
                store_data => StoreData
            },
            
            %% Emit telemetry
            telemetry:execute([presence_api, rest, get], 
                            #{count => 1}, 
                            #{user_id => UserId}),
            
            Body = jsx:encode(Response),
            {Body, Req, State};
            
        {error, not_found} ->
            %% User not online, check store
            case presence_store:get_user(UserId) of
                {ok, StoreData} ->
                    %% User exists in store but not online
                    Response = #{
                        user_id => UserId,
                        status => offline,
                        connected_at => null,
                        last_heartbeat => maps:get(last_seen, StoreData, null),
                        uptime_seconds => 0,
                        metadata => #{},
                        store_data => StoreData
                    },
                    Body = jsx:encode(Response),
                    {Body, Req, State};
                    
                {error, not_found} ->
                    %% User doesn't exist at all
                    {false, Req, State}
            end
    end.

%% @doc
%% Handle PUT request - update user presence.
%% @end
update_presence(Req, #state{user_id = UserId} = State) ->
    %% Read request body
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    
    try
        %% Parse JSON body
        Data = jsx:decode(Body, [return_maps]),
        
        %% Validate data
        case validate_presence_data(Data) of
            ok ->
                %% Start user process if not exists
                case presence_user_sup:start_user(UserId) of
                    {ok, _Pid} ->
                        %% Update metadata if provided
                        case maps:get(<<"metadata">>, Data, undefined) of
                            undefined -> ok;
                            Metadata -> presence_user:update_metadata(UserId, Metadata)
                        end,
                        
                        %% Send heartbeat to mark as online
                        presence_user:heartbeat(UserId),
                        
                        %% Store additional data if provided
                        case maps:get(<<"store_data">>, Data, undefined) of
                            undefined -> ok;
                            StoreData -> 
                                presence_store:save_user(UserId, StoreData#{
                                    last_seen => erlang:system_time(millisecond)
                                })
                        end,
                        
                        %% Emit telemetry
                        telemetry:execute([presence_api, rest, update], 
                                        #{count => 1}, 
                                        #{user_id => UserId}),
                        
                        %% Return success
                        Req3 = cowboy_req:set_resp_header(<<"content-type">>, 
                                                         <<"application/json">>, 
                                                         Req2),
                        Req4 = cowboy_req:set_resp_body(jsx:encode(#{
                            success => true,
                            user_id => UserId
                        }), Req3),
                        {true, Req4, State};
                        
                    {error, Reason} ->
                        %% Failed to start user process
                        error_response(Req2, 500, <<"internal_error">>, 
                                     iolist_to_binary(io_lib:format("~p", [Reason])))
                end;
                
            {error, ValidationError} ->
                error_response(Req2, 400, <<"validation_error">>, ValidationError)
        end
    catch
        error:badarg ->
            error_response(Req2, 400, <<"invalid_json">>, <<"Invalid JSON in request body">>)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Validate presence update data.
%% @end
-spec validate_presence_data(Data :: map()) -> ok | {error, binary()}.
validate_presence_data(Data) when is_map(Data) ->
    %% Check metadata if provided
    case maps:get(<<"metadata">>, Data, undefined) of
        undefined -> ok;
        Meta when is_map(Meta) -> ok;
        _ -> {error, <<"metadata must be an object">>}
    end,
    
    %% Check store_data if provided
    case maps:get(<<"store_data">>, Data, undefined) of
        undefined -> ok;
        Store when is_map(Store) -> ok;
        _ -> {error, <<"store_data must be an object">>}
    end;
validate_presence_data(_) ->
    {error, <<"request body must be an object">>}.

%% @private
%% @doc
%% Send error response.
%% @end
-spec error_response(Req, StatusCode, Error, Message) -> {stop, Req, State} when
    Req :: cowboy_req:req(),
    StatusCode :: pos_integer(),
    Error :: binary(),
    Message :: binary(),
    State :: any().
error_response(Req, StatusCode, Error, Message) ->
    Body = jsx:encode(#{
        error => Error,
        message => Message
    }),
    Req2 = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req),
    {stop, Req2, undefined}.