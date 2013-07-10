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
-spec get_messages(binary(), binary()) -> {ok, undefined}.
get_messages(Channel, Consumer_code) ->
    {ok, Consumer_code1, Consumer_pid} = get_or_create_consumer(Consumer_code),
    {ok, Messages} = s_consumer:get_messages(Consumer_pid, Channel),
    {ok, Consumer_code1, Messages}.


publish(Channel, Messages)->
    %% {ok, User_pid} = get_or_create_user(User_code),
    {ok, Channel_pid} = s_channel:get(Channel),
    ok = s_channel:publish(Channel_pid, Messages),
    ok.

subscribe(Channel, Consumer_code) ->
    error_logger:info_report({subscribe, Channel, Consumer_code}),

    {ok, Consumer_code1, Consumer_pid} = get_or_create_consumer(Consumer_code),
    {ok, Channel_pid} = s_channel:get(Channel),
    ok = s_consumer:subscribe(Consumer_pid, Channel_pid),
    {ok, Consumer_code1}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_or_create_consumer(Code)->
    case Code of
	undefined -> Consumer_code = s_utils:generate_code();
	Code -> Consumer_code = Code
    end,
    case s_consumer:get(Consumer_code) of
	{error, not_found} -> {ok, Consumer} = s_consumer_sup:start_child(Consumer_code),
			      {ok,Consumer_code,  Consumer};
	{ok, Pid} -> {ok, Consumer_code, Pid}
    end.



				      
