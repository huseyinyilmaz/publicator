%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>

-module(server).


%% API
-export([start/0, stop/0]).
-export([get_messages/2, publish/2, subscribe/2]).

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
-spec get_messages(binary(), binary()) -> undefined.
get_messages(Channel, Consumer_code) ->
    {ok, Consumer_pid} = get_or_create_consumer(Consumer_code),
    s_consumer:get_messages(Consumer_pid, Channel).


publish(Channel, Messages)->
    %% {ok, User_pid} = get_or_create_user(User_code),
    {ok, Channel_pid} = s_channel:get(Channel),
    ok = s_channel:publish(Channel_pid, Messages),
    {ok, <<"session_id_value">>}.

subscribe(Channel, Consumer_code) ->
    {ok, Consumer_pid} = get_or_create_consumer(Consumer_code),
    {ok, Channel_pid} = s_channel:get(Channel),
    s_consumer:subscribe(Consumer_pid, Channel_pid).

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_or_create_consumer(Code)->
    case Code of
	undefined -> User_code = s_utils:generate_code();
	Code -> User_code = Code
    end,
    case s_user:get_user(User_code) of
	{error, not_found} -> {ok, User} = s_user_sup:start_child(),
			      {ok, User};
	{ok, Pid} -> {ok, Pid}
    end.



				      
