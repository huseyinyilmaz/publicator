%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>

-module(server).


%% API
-export([start/0, stop/0]).
-export([get_messages/1, publish/4,
	 subscribe/4, unsubscribe/2,
	 get_subscribtions/1,
	 create_consumer/2, get_consumer/1, get_channels/0]).
-export([stop_consumer/1]).
-export([get_consumers/3]).
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
    ok = s_utils:ensure_started(lager),
    ok = s_utils:ensure_started(ibrowse),
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



get_channels() ->
    {error, not_implemented}.
%%--------------------------------------------------------------------
%% @doc
%% Stops pub-sub server
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_messages(binary()) -> {ok, [tuple()]} | {error, consumer_not_found}.
get_messages(Consumer_code) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
	    s_consumer:get_messages(Consumer_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec publish(binary(), binary(), binary(), list()) -> ok
                                                           | {error, consumer_not_found}
                                                           | {error, permission_denied}.
publish(Consumer_code, Channel_code, Message, Extra_data)->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
            %% returns ok or permission denied
	    s_consumer:publish(Consumer_pid, Channel_code, Message, Extra_data);
	{error, not_found} -> {error, consumer_not_found}
    end.

-spec subscribe(binary(), binary(), channel_handler_type(), term()) ->
                       ok  | {error, invalid_channel_code}
                           | {error, consumer_not_found}
                           | {error, permission_denied}.
subscribe(Consumer_code, Channel_code, Handler_type, Extra_data) ->
    case is_channel_code_valid(Channel_code) of
	false -> {error, invalid_channel_code};
	true ->
	    case s_consumer:get(Consumer_code) of
                {ok, Consumer_pid} ->
                    s_consumer:subscribe(Consumer_pid,
                                         Channel_code,
                                         Handler_type,
                                         Extra_data);
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

-spec get_subscribtions(binary()) -> {ok, dict:dict(binary(), pid())} |
                                     {error, consumer_not_found}.
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


-spec create_consumer(Auth_info::binary(),
                      Extra_data::term()) -> {ok, Code::binary(), Pid::pid()}
                                                 | {error, permission_denied}.
create_consumer(Auth_info, Extra_data) ->
    s_consumer_sup:start_child(Auth_info, Extra_data).

-spec stop_consumer(binary()) -> ok|{error, consumer_not_found}.
stop_consumer(Consumer_code) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
            s_consumer:stop(Consumer_pid);
	{error, not_found} -> {error, consumer_not_found}
    end.


-spec get_consumer(binary()) -> {ok, pid()} | {error, not_found}.
get_consumer(Consumer_code) ->
    s_consumer:get(Consumer_code).

-spec get_consumers(binary(), binary(), list()) ->
                           {ok, [pid()]}
                               | {error, consumer_not_found}
                               | {error, permission_denied}
                               | {error, invalid_channel_code}.
get_consumers(Consumer_code, Channel_code, Extra_data) ->
    case s_consumer:get(Consumer_code) of
	{ok, Consumer_pid} ->
            s_consumer:get_consumers(Consumer_pid, Channel_code, Extra_data);
	{error, not_found} -> {error, consumer_not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec is_channel_code_valid(binary()) -> boolean().
is_channel_code_valid(Channel_code) ->
    case re:run(Channel_code,"^[0-9a-z_]*$",[{capture, none}]) of
	match -> true;
	nomatch -> false
	end.
