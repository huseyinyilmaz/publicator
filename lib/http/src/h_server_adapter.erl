%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% adapter between http and server apps
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(h_server_adapter).

%% API
-export([subscribe/3, unsubscribe/2,
	 get_channels/0, get_subscribtions/1,
	 get_messages/1, publish/3, create_consumer/0,
	 get_consumer/1, get_consumers/1]).
-export([stop_consumer/1]).
-export([add_message_handler/2, remove_message_handler/2]).

%%%===================================================================
%%% API
%%%===================================================================
    
subscribe(Session_id, Channel_code, Handler_type)->
    server:subscribe(Session_id, Channel_code, Handler_type).
%% subscribe(Session_id, Channel_code, Handler)-> server:subscribe(Session_id,
%% 								Channel_code,
%% 								Handler).

unsubscribe(Session_id, Channel_code)-> server:unsubscribe(Session_id, Channel_code).
%% unsubscribe(Session_id, Channel_code, Handler)-> server:unsubscribe(Session_id,
%% 								    Channel_code,
%% 								    Handler).

get_channels()-> server:get_channels().

get_consumer(Consumer_code)-> server:get_consumer(Consumer_code).

get_subscribtions(Session_id) -> server:get_subscribtions(Session_id).
get_messages(Session_id) -> server:get_messages(Session_id).
publish(Session_id, Channel_code, Message) -> server:publish(Session_id, Channel_code, Message).
create_consumer() -> server:create_consumer().
stop_consumer(Consumer_code) -> server:stop_consumer(Consumer_code).
get_consumers(Channel_code) -> server:get_consumers(Channel_code).
    
add_message_handler(Session_id, Handler_pid) -> server:add_message_handler(Session_id,
									   Handler_pid).
remove_message_handler(Session_id, Handler_pid) -> server:remove_message_handler(Session_id,
										 Handler_pid).
    
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
