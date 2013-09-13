%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>

-module(server).


%% API
-export([start/0, stop/0]).
-export([get_messages/2, get_messages/1, publish/3,
	 subscribe/2, unsubscribe/2,
	 get_channels/0, get_subscribtions/1,
	 create_consumer/0]).

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
    ok = s_utils:ensure_started(server).


%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    ok = application:stop(server).


%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_messages(binary(), binary()) -> {ok, [binary()]} | {error, consumer_not_found}.
get_messages(Consumer_code, Channel_code) ->
    case s_manager:get_consumer(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:get_messages(Consumer_pid, Channel_code);
	{error, not_found} -> {error, consumer_not_found}
    end.
 
-spec get_messages(binary()) -> {ok, [binary()]} | {error, consumer_not_found}.
get_messages(Consumer_code) ->
    case s_manager:get_consumer(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:get_messages(Consumer_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec publish(binary(), binary(), binary()) -> ok | {error, consumer_not_found}.
publish(Consumer_code, Channel_code, Message)->
    case s_manager:get_consumer(Consumer_code) of
	{ok, Consumer_pid} ->
	    ok = s_consumer:publish(Consumer_pid, Channel_code, Message);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec subscribe(binary(), binary()) -> ok | {error, consumer_not_found}.
subscribe(Consumer_code, Channel_code) ->
    case s_manager:get_consumer(Consumer_code) of
	{ok, Consumer_pid} ->
	    ok = s_consumer:subscribe(Consumer_pid, Channel_code);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec unsubscribe(binary(), binary()) -> ok | {error, consumer_not_found}.
unsubscribe(Consumer_code, Channel_code) ->
    case s_manager:get_consumer(Consumer_code) of
	{ok, Consumer_pid} ->
	    ok = s_consumer:unsubscribe(Consumer_pid, Channel_code);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec get_channels() -> {ok, [binary()]}.
get_channels() ->
    {ok, s_manager:get_channels()}.


-spec get_subscribtions(binary()) -> {ok, [binary()]} | {error, consumer_not_found}.
get_subscribtions(Consumer_code) ->
    case s_manager:get_consumer(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:get_subscribtions(Consumer_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec create_consumer() -> {ok, binary(), binary()}.
create_consumer() ->
    s_manager:create_consumer().

%%%===================================================================
%%% Internal functions
%%%===================================================================
