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
    Consumer_pid = get_consumer(Consumer_code),
    s_consumer:get_messages(Consumer_pid).

publish(Channel_code, Message)->
    %% {ok, User_pid} = get_or_create_user(User_code),
    Channel_pid = get_channel(Channel_code),
    ok = s_channel:publish(Channel_pid, Message),
    ok.

subscribe(Channel_code, Consumer_code) ->
    error_logger:info_report({subscribe, Channel_code, Consumer_code}),
    Channel_pid = get_channel(Channel_code),
    Consumer_pid = get_consumer(Consumer_code),
    ok = s_manager:bind(Channel_code, Consumer_code),
    s_channel:add_handler(Channel_pid, Channel_code, Consumer_pid),
    ok.

unsubscribe(Channel_code, Consumer_code) ->
    error_logger:info_report({unsubscribe, Channel_code, Consumer_code}),
    Channel_pid = get_channel(Channel_code),
    Consumer_pid = get_consumer(Consumer_code),
    ok = s_manager:unbind(Channel_code, Consumer_code),
    s_channel:delete_handler(Channel_pid, Consumer_pid),
    ok.

get_channels() ->
    {ok, s_manager:get_channels()}.

get_subscribtions(Consumer_code) ->
    {ok, s_manager:get_subscribtions(Consumer_code)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
				      
get_channel(Channel_code)->
    case s_manager:get_channel(Channel_code) of
	{error, not_found} -> {ok, Channel_pid} = s_channel_sup:start_child(),
			      s_manager:add_channel(Channel_code, Channel_pid);
	{ok, Pid} -> Channel_pid = Pid
    end,
    Channel_pid.

get_consumer(Consumer_code)->
    case s_manager:get_consumer(Consumer_code) of
	{error, not_found} -> {ok, Consumer_pid} = s_consumer_sup:start_child(Consumer_code),
			      s_manager:add_consumer(Consumer_code, Consumer_pid);
	{ok, Pid} -> Consumer_pid = Pid
    end,
    Consumer_pid.


		     
						     

