%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>

-module(server).


%% API
-export([start/0, stop/0]).
-export([get_messages/2, get_messages/1, publish/2,
	 subscribe/2, unsubscribe/2,
	 get_channels/0, get_subscribtions/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts pub-sub server
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    application:start(server).


%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    application:stop(server).


%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_messages(binary(), binary()) -> {ok, undefined}.
get_messages(Consumer_code, Channel) ->
    %% {ok, Consumer_code1, Consumer_pid} = get_or_create_consumer(Consumer_code),
    %% {ok, Messages} = s_consumer:get_messages(Consumer_pid, Channel),
    %% {ok, Consumer_code1, Messages}.
    ok.

-spec get_messages(binary()) -> {ok, undefined}.
get_messages(Consumer_code) ->
    %% Consumer_pid = get_consumer(Consumer_code),
    %% s_consumer:get_messages(Consumer_pid).
    ok.

publish(Channel_code, Message)->
    %% {ok, User_pid} = get_or_create_user(User_code),
    %% Channel_pid = get_channel(Channel_code),
    %% ok = s_channel:publish(Channel_pid, Message),
    ok.

subscribe(Channel_code, Consumer_code) ->
    error_logger:info_report({subscribe, Channel_code, Consumer_code}),
    {ok, Consumer_pid} = s_manager:get_or_create_consumer(Consumer_code),
    ok = s_consumer:subscribe(Consumer_pid, Channel_code),
    ok.

unsubscribe(Channel_code, Consumer_code) ->
    error_logger:info_report({unsubscribe, Channel_code, Consumer_code}),
    {ok, Consumer_pid} = s_manager:get_or_create_consumer(Consumer_code),
    ok = s_consumer:unsubscribe(Consumer_pid, Channel_code),
    ok.

get_channels() ->
    {ok, s_manager:get_channels()}.

get_subscribtions(Consumer_code) ->
    {ok, Consumer_pid} = s_manager:get_or_create_consumer(Consumer_code),
    s_consumer:get_subscribsions(Consumer_pid).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
