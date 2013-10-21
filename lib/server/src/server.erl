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
	 subscribe/3, unsubscribe/2,
	 get_subscribtions/1,
	 create_consumer/0, get_consumer/1, get_channels/0]).
-export([get_consumers/1]).
-export([add_message_handler/2, remove_message_handler/2]).


-include_lib("stdlib/include/qlc.hrl").
-include("../include/server.hrl").
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



-spec get_channels() -> {ok, [code()]}.
get_channels() ->
    Q1 = qlc:q([Channel_code|| {{n,l,{channel, Channel_code}}, _, undefined }
				   <- gproc:table(names)]),
    Channel_code_list = qlc:e(Q1),
    {ok, Channel_code_list}.
%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_messages(binary(), binary()) -> {ok, [binary()]} | {error, consumer_not_found}.
get_messages(Consumer_code, Channel_code) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:get_messages(Consumer_pid, Channel_code);
	{error, not_found} -> {error, consumer_not_found}
    end.
 
-spec get_messages(binary()) -> {ok, [binary()]} | {error, consumer_not_found}.
get_messages(Consumer_code) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:get_messages(Consumer_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec publish(binary(), binary(), binary()) -> ok | {error, consumer_not_found}.
publish(Consumer_code, Channel_code, Message)->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    ok = s_consumer:publish(Consumer_pid, Channel_code, Message);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec subscribe(binary(), binary(), channel_handler_type()) ->
		       ok | {error, consumer_not_found}.
subscribe(Consumer_code, Channel_code, Handler_type) ->
    case is_channel_code_valid(Channel_code) of
	false -> {error, invalid_channel_code};
	true ->
	    case s_consumer:get(Consumer_code) of
		{ok, Consumer_pid} ->
		    ok = s_consumer:subscribe(Consumer_pid, Channel_code, Handler_type);
		{error, not_found} -> {error, consumer_not_found}
	    end
    end.

-spec unsubscribe(binary(), binary()) -> ok | {error, consumer_not_found}.
unsubscribe(Consumer_code, Channel_code) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    ok = s_consumer:unsubscribe(Consumer_pid, Channel_code);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec get_subscribtions(binary()) -> {ok, [binary()]} | {error, consumer_not_found}.
get_subscribtions(Consumer_code) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:get_subscribtions(Consumer_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec add_message_handler(binary(), pid()) -> ok.
add_message_handler(Consumer_code, Handler_pid) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:add_message_handler(Consumer_pid, Handler_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec remove_message_handler(binary(), pid()) -> ok.
remove_message_handler(Consumer_code, Handler_pid) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:remove_message_handler(Consumer_pid, Handler_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.


-spec create_consumer() -> {ok, Code::binary(), Pid::binary()}.
create_consumer() ->
    s_consumer_sup:start_child().


-spec get_consumer(binary()) -> {ok, pid()} | {error, not_found}.
get_consumer(Consumer_code) ->
    s_consumer:get(Consumer_code).

-spec get_consumers(binary()) -> {ok, pid()} | {error, not_found}.
get_consumers(Channel_code) ->
    case s_channel:get_channel(Channel_code) of
	{ok, Consumer_pid} ->
	    s_channel:get_consumers(Consumer_pid);
	{error, not_found} -> {error, channel_not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_channel_code_valid(Channel_code) ->
    case re:run(Channel_code,"^[0-9a-z_]*$",[{capture, none}]) of
	match -> true;
	nomatch -> false
	end.
