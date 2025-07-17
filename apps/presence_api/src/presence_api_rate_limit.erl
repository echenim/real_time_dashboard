%%%-------------------------------------------------------------------
%%% @author Real-Time Dashboard Team
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Rate limiting for API endpoints and WebSocket connections.
%%%
%%% Uses token bucket algorithm with ETS for storage.
%%% Configurable limits per IP address.
%%%
%%% Design Decision: Token bucket algorithm
%%% Trade-off: Token bucket vs sliding window vs fixed window
%%% - Chosen: Token bucket for:
%%%   * Smooth rate limiting
%%%   * Burst allowance
%%%   * Simple implementation
%%% - Alternative: Sliding window is more accurate but complex
%%%
%%% Design Decision: IP-based limiting
%%% Trade-off: IP vs user vs combination
%%% - Chosen: IP for simplicity and DDoS protection
%%% - Future: Can add user-based limits on top
%%% @end
%%%-------------------------------------------------------------------
-module(presence_api_rate_limit).

%% API
-export([
    init/0,
    cleanup/0,
    check_request/2,
    check_ws_connection/1,
    check_ws_message/2,
    get_limits/1,
    reset_limits/1
]).

%% Table names
-define(REQUEST_BUCKETS, rate_limit_requests).
-define(WS_CONNECTIONS, rate_limit_ws_connections).
-define(WS_MESSAGES, rate_limit_ws_messages).

%% Bucket record
-record(bucket, {
    key :: term(),
    tokens :: float(),
    last_update :: integer()  % milliseconds
}).

%% Connection record
-record(connection_count, {
    ip :: inet:ip_address(),
    count :: non_neg_integer()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Initialize rate limiting tables.
%% @end
-spec init() -> ok.
init() ->
    %% Create ETS tables
    %% Trade-off: Multiple tables vs single table with composite keys
    %% - Chosen: Multiple tables for better concurrent access
    
    %% Table for HTTP request rate limiting
    ets:new(?REQUEST_BUCKETS, [
        named_table,
        public,
        set,
        {keypos, #bucket.key},
        {write_concurrency, true}
    ]),
    
    %% Table for WebSocket connection counting
    ets:new(?WS_CONNECTIONS, [
        named_table,
        public,
        set,
        {keypos, #connection_count.ip},
        {write_concurrency, true}
    ]),
    
    %% Table for WebSocket message rate limiting
    ets:new(?WS_MESSAGES, [
        named_table,
        public,
        set,
        {keypos, #bucket.key},
        {write_concurrency, true}
    ]),
    
    %% Schedule periodic cleanup
    schedule_cleanup(),
    
    lager:info("Rate limiting initialized"),
    ok.

%% @doc
%% Clean up rate limiting tables.
%% @end
-spec cleanup() -> ok.
cleanup() ->
    catch ets:delete(?REQUEST_BUCKETS),
    catch ets:delete(?WS_CONNECTIONS),
    catch ets:delete(?WS_MESSAGES),
    ok.

%% @doc
%% Check if an HTTP request is allowed.
%%
%% Returns ok if allowed, {error, rate_limited} if not.
%% @end
-spec check_request(IP :: inet:ip_address(), Endpoint :: binary()) -> 
    ok | {error, rate_limited}.
check_request(IP, Endpoint) ->
    %% Get configured limits
    MaxRequests = presence_api_app:get_env(max_requests_per_minute, 1000),
    
    %% Create bucket key
    Key = {IP, Endpoint},
    
    %% Check token bucket
    case consume_token(?REQUEST_BUCKETS, Key, MaxRequests, 60000) of
        true -> ok;
        false -> {error, rate_limited}
    end.

%% @doc
%% Check if a new WebSocket connection is allowed.
%% @end
-spec check_ws_connection(IP :: inet:ip_address()) -> 
    ok | {error, rate_limited}.
check_ws_connection(IP) ->
    %% Get configured limit
    MaxConnections = presence_api_app:get_env(max_connections_per_ip, 100),
    
    %% Get current connection count
    Count = case ets:lookup(?WS_CONNECTIONS, IP) of
        [#connection_count{count = C}] -> C;
        [] -> 0
    end,
    
    %% Check limit
    if
        Count >= MaxConnections ->
            {error, rate_limited};
        true ->
            %% Increment count
            %% Trade-off: update_counter vs insert
            %% - Chosen: update_counter for atomicity
            try
                ets:update_counter(?WS_CONNECTIONS, IP, {#connection_count.count, 1})
            catch
                error:badarg ->
                    %% First connection from this IP
                    ets:insert(?WS_CONNECTIONS, #connection_count{ip = IP, count = 1})
            end,
            ok
    end.

%% @doc
%% Check if a WebSocket message is allowed.
%% @end
-spec check_ws_message(IP :: inet:ip_address(), UserId :: binary()) -> 
    ok | {error, rate_limited}.
check_ws_message(IP, UserId) ->
    %% Get configured limit
    MaxMessages = presence_api_app:get_env(ws_messages_per_minute, 600),
    
    %% Create bucket key (combine IP and user for fairness)
    Key = {IP, UserId},
    
    %% Check token bucket
    case consume_token(?WS_MESSAGES, Key, MaxMessages, 60000) of
        true -> ok;
        false -> {error, rate_limited}
    end.

%% @doc
%% Get current rate limit status for an IP.
%% @end
-spec get_limits(IP :: inet:ip_address()) -> map().
get_limits(IP) ->
    %% Get connection count
    ConnCount = case ets:lookup(?WS_CONNECTIONS, IP) of
        [#connection_count{count = C}] -> C;
        [] -> 0
    end,
    
    %% Get request bucket info
    %% This is approximate since we have per-endpoint buckets
    RequestBuckets = ets:match_object(?REQUEST_BUCKETS, #bucket{key = {IP, '_'}, _ = '_'}),
    
    %% Calculate total tokens across endpoints
    TotalTokens = lists:foldl(
        fun(#bucket{tokens = T}, Acc) -> Acc + T end,
        0,
        RequestBuckets
    ),
    
    #{
        ip => IP,
        ws_connections => ConnCount,
        ws_connection_limit => presence_api_app:get_env(max_connections_per_ip, 100),
        approximate_request_tokens => TotalTokens,
        request_limit => presence_api_app:get_env(max_requests_per_minute, 1000)
    }.

%% @doc
%% Reset rate limits for an IP (admin function).
%% @end
-spec reset_limits(IP :: inet:ip_address()) -> ok.
reset_limits(IP) ->
    %% Delete all buckets for this IP
    ets:match_delete(?REQUEST_BUCKETS, #bucket{key = {IP, '_'}, _ = '_'}),
    ets:match_delete(?WS_MESSAGES, #bucket{key = {IP, '_'}, _ = '_'}),
    
    %% Reset connection count
    ets:delete(?WS_CONNECTIONS, IP),
    
    lager:info("Reset rate limits for IP ~p", [IP]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Consume a token from a bucket using token bucket algorithm.
%%
%% Returns true if token was consumed, false if bucket is empty.
%% @end
-spec consume_token(Table :: atom(), Key :: term(), 
                   MaxTokens :: pos_integer(), 
                   RefillPeriodMs :: pos_integer()) -> boolean().
consume_token(Table, Key, MaxTokens, RefillPeriodMs) ->
    Now = erlang:monotonic_time(millisecond),
    
    %% Get or create bucket
    Bucket = case ets:lookup(Table, Key) of
        [B] -> B;
        [] -> #bucket{key = Key, tokens = MaxTokens, last_update = Now}
    end,
    
    %% Calculate tokens to add based on time elapsed
    %% Trade-off: Continuous vs discrete refill
    %% - Chosen: Continuous for smoother rate limiting
    TimeDelta = Now - Bucket#bucket.last_update,
    RefillRate = MaxTokens / RefillPeriodMs,  % tokens per millisecond
    TokensToAdd = TimeDelta * RefillRate,
    
    %% Update token count (cap at max)
    NewTokens = min(Bucket#bucket.tokens + TokensToAdd, MaxTokens),
    
    %% Try to consume a token
    if
        NewTokens >= 1 ->
            %% Consume token and update bucket
            UpdatedBucket = Bucket#bucket{
                tokens = NewTokens - 1,
                last_update = Now
            },
            ets:insert(Table, UpdatedBucket),
            true;
        true ->
            %% Not enough tokens, just update timestamp
            UpdatedBucket = Bucket#bucket{
                tokens = NewTokens,
                last_update = Now
            },
            ets:insert(Table, UpdatedBucket),
            false
    end.

%% @private
%% @doc
%% Schedule periodic cleanup of old entries.
%%
%% Prevents memory leak from abandoned IPs.
%% @end
-spec schedule_cleanup() -> ok.
schedule_cleanup() ->
    %% Run cleanup every 5 minutes
    timer:apply_interval(300000, ?MODULE, cleanup_old_entries, []),
    ok.

%% @private
%% @doc
%% Clean up old rate limit entries.
%% @end
-spec cleanup_old_entries() -> ok.
cleanup_old_entries() ->
    Now = erlang:monotonic_time(millisecond),
    CutoffTime = Now - 3600000,  % 1 hour ago
    
    %% Clean request buckets
    ets:select_delete(?REQUEST_BUCKETS, [
        {#bucket{last_update = '$1', _ = '_'},
         [{'<', '$1', CutoffTime}],
         [true]}
    ]),
    
    %% Clean message buckets
    ets:select_delete(?WS_MESSAGES, [
        {#bucket{last_update = '$1', _ = '_'},
         [{'<', '$1', CutoffTime}],
         [true]}
    ]),
    
    ok.