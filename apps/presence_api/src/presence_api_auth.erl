%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Authentication middleware and JWT handling.
%%%
%%% This module provides:
%%% - JWT token verification
%%% - Cowboy middleware for HTTP authentication
%%% - Token generation utilities
%%%
%%% Design Decision: JWT for stateless authentication
%%% Trade-off: JWT vs sessions
%%% - Chosen: JWT for:
%%%   * Stateless authentication
%%%   * Easy horizontal scaling
%%%   * Standard format
%%% - Downside: Can't revoke tokens (must wait for expiry)
%%% @end
%%%-------------------------------------------------------------------
-module(presence_api_auth).

-behaviour(cowboy_middleware).

%% Cowboy middleware callbacks
-export([execute/2]).

%% API
-export([
    verify_token/1,
    generate_token/1,
    generate_token/2,
    extract_token_from_req/1,
    is_public_endpoint/1
]).

%% Default token expiry (1 hour)
-define(DEFAULT_EXPIRY, 3600).

%%%===================================================================
%%% Cowboy middleware callbacks
%%%===================================================================

%% @doc
%% Execute authentication middleware.
%%
%% Checks if the endpoint requires authentication and validates JWT.
%% @end
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req} when
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env().
execute(Req, Env) ->
    %% Get request path
    Path = cowboy_req:path(Req),
    
    %% Check if endpoint is public
    case is_public_endpoint(Path) of
        true ->
            %% Public endpoint, skip authentication
            {ok, Req, Env};
        false ->
            %% Check if auth is enabled
            case presence_api_app:get_env(auth_enabled, true) of
                false ->
                    %% Auth disabled (development mode)
                    {ok, Req, Env};
                true ->
                    %% Verify authentication
                    authenticate_request(Req, Env)
            end
    end.

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Verify a JWT token.
%%
%% Returns the claims if valid, error otherwise.
%% @end
-spec verify_token(Token :: binary()) -> {ok, map()} | {error, term()}.
verify_token(Token) when is_binary(Token) ->
    %% Get JWT secret and algorithm
    Secret = presence_api_app:get_env(jwt_secret, <<"change_me_in_production">>),
    _Algorithm = presence_api_app:get_env(jwt_algorithm, hs256),
    
    %% Create JWK (JSON Web Key)
    JWK = #{
        <<"kty">> => <<"oct">>,
        <<"k">> => jose_base64url:encode(Secret)
    },
    
    try
        %% Verify and decode token
        case jose_jwt:verify(JWK, Token) of
            {true, JWT, _JWS} ->
                %% Extract claims
                Claims = maps:get(fields, JWT, #{}),
                
                %% Check expiration
                case check_expiration(Claims) of
                    ok ->
                        {ok, Claims};
                    expired ->
                        {error, token_expired}
                end;
            {false, _JWT, _JWS} ->
                {error, invalid_signature}
        end
    catch
        _:_ ->
            {error, invalid_token}
    end;
verify_token(_) ->
    {error, invalid_token}.

%% @doc
%% Generate a JWT token for a user.
%% @end
-spec generate_token(UserId :: binary()) -> {ok, binary()} | {error, term()}.
generate_token(UserId) ->
    generate_token(UserId, ?DEFAULT_EXPIRY).

%% @doc
%% Generate a JWT token with custom expiry.
%% @end
-spec generate_token(UserId :: binary(), ExpirySeconds :: pos_integer()) -> 
    {ok, binary()} | {error, term()}.
generate_token(UserId, ExpirySeconds) when is_binary(UserId), is_integer(ExpirySeconds) ->
    %% Get configuration
    Secret = presence_api_app:get_env(jwt_secret, <<"change_me_in_production">>),
    Algorithm = presence_api_app:get_env(jwt_algorithm, hs256),
    
    %% Create claims
    Now = erlang:system_time(second),
    Claims = #{
        <<"sub">> => UserId,                    % Subject (user ID)
        <<"iat">> => Now,                       % Issued at
        <<"exp">> => Now + ExpirySeconds,       % Expiration
        <<"iss">> => <<"presence_dashboard">>,  % Issuer
        <<"aud">> => <<"presence_api">>         % Audience
    },
    
    %% Create JWT
    JWT = jose_jwt:from_map(Claims),
    
    %% Create JWK
    JWK = #{
        <<"kty">> => <<"oct">>,
        <<"k">> => jose_base64url:encode(Secret)
    },
    
    %% Sign token
    %% Trade-off: Algorithm choice
    %% - HS256: Symmetric, fast, good for single service
    %% - RS256: Asymmetric, slower, better for microservices
    {_Alg, Token} = jose_jwt:sign(JWK, #{<<"alg">> => atom_to_binary(Algorithm, utf8)}, JWT),
    
    {ok, Token}.

%% @doc
%% Extract JWT token from Cowboy request.
%% @end
-spec extract_token_from_req(Req :: cowboy_req:req()) -> binary() | undefined.
extract_token_from_req(Req) ->
    %% Try Authorization header first
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            Token;
        _ ->
            %% Try query parameter
            QsVals = cowboy_req:parse_qs(Req),
            case lists:keyfind(<<"token">>, 1, QsVals) of
                {_, Token} -> Token;
                false ->
                    %% Try cookie
                    Cookies = cowboy_req:parse_cookies(Req),
                    case lists:keyfind(<<"auth_token">>, 1, Cookies) of
                        {_, Token} -> Token;
                        false -> undefined
                    end
            end
    end.

%% @doc
%% Check if an endpoint is public (no auth required).
%% @end
-spec is_public_endpoint(Path :: binary()) -> boolean().
is_public_endpoint(<<"/health">>) -> true;
is_public_endpoint(<<"/health/", _/binary>>) -> true;
is_public_endpoint(<<"/ws">>) -> true;  % WebSocket does its own auth
is_public_endpoint(<<"/static/", _/binary>>) -> true;
is_public_endpoint(<<"/">>) -> true;
is_public_endpoint(_) -> false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Authenticate a request.
%% @end
-spec authenticate_request(Req, Env) -> {ok, Req, Env} | {stop, Req} when
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env().
authenticate_request(Req, Env) ->
    case extract_token_from_req(Req) of
        undefined ->
            %% No token provided
            unauthorized_response(Req, <<"No authentication token provided">>);
        Token ->
            case verify_token(Token) of
                {ok, Claims} ->
                    %% Add claims to request context
                    Req2 = cowboy_req:set_resp_header(<<"x-user-id">>, 
                                                     maps:get(<<"sub">>, Claims, <<>>), 
                                                     Req),
                    
                    %% Store claims in Env for handlers
                    Env2 = maps:put(auth_claims, Claims, Env),
                    
                    %% Emit telemetry
                    telemetry:execute([presence_api, auth, success], 
                                    #{count => 1}, 
                                    #{user_id => maps:get(<<"sub">>, Claims, undefined)}),
                    
                    {ok, Req2, Env2};
                {error, token_expired} ->
                    unauthorized_response(Req, <<"Token expired">>);
                {error, _} ->
                    unauthorized_response(Req, <<"Invalid token">>)
            end
    end.

%% @private
%% @doc
%% Send 401 Unauthorized response.
%% @end
-spec unauthorized_response(Req, Message) -> {stop, Req} when
    Req :: cowboy_req:req(),
    Message :: binary().
unauthorized_response(Req, Message) ->
    %% Emit telemetry
    telemetry:execute([presence_api, auth, failure], #{count => 1}, #{}),
    
    Body = jsx:encode(#{
        error => <<"unauthorized">>,
        message => Message
    }),
    
    Req2 = cowboy_req:reply(401, #{
        <<"content-type">> => <<"application/json">>,
        <<"www-authenticate">> => <<"Bearer">>
    }, Body, Req),
    
    {stop, Req2}.

%% @private
%% @doc
%% Check if token is expired.
%% @end
-spec check_expiration(Claims :: map()) -> ok | expired.
check_expiration(Claims) ->
    case maps:get(<<"exp">>, Claims, undefined) of
        undefined ->
            %% No expiration claim
            ok;
        Exp when is_integer(Exp) ->
            Now = erlang:system_time(second),
            if
                Now > Exp -> expired;
                true -> ok
            end;
        _ ->
            %% Invalid expiration format
            expired
    end.