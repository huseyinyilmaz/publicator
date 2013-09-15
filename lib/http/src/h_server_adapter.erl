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
-export([subscribe/2, unsubscribe/2, get_channels/0, get_subscribtions/1,
	 get_messages/1, publish/3, create_consumer/0]).

-export([handle_request/3]).
%%%===================================================================
%%% API
%%%===================================================================
    
subscribe(Session_id, Channel_code)-> server:subscribe(Session_id, Channel_code).
unsubscribe(Session_id, Channel_code)-> server:unsubscribe(Session_id, Channel_code).
get_channels()-> server:get_channels().
get_subscribtions(Session_id) -> server:get_subscribtions(Session_id).
get_messages(Session_id) -> server:get_messages(Session_id).
publish(Session_id, Channel_code, Message)-> server:publish(Session_id, Channel_code, Message).
create_consumer() -> server:create_consumer().



%%%===================================================================
%%% WEBSOCKET HANDLERS
%%%===================================================================
handle_request(Msg, Req, State)->
    Result=jiffy:encode({[{<<"type">>,<<"unhandled_msg">>},{<<"msg">>, Msg}]}),
    {reply, Result, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
