%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>

-module(server).


%% API
-export([start/0, stop/0]).
-export([get_messages/2, publish/3]).

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
get_messages(User_code, Resource_name) ->
    {ok, User_pid} = get_or_create_user(User_code),
    s_user:get_messages(User_pid, Resource_name).


publish(Resource_name, Messages)->
    %% {ok, User_pid} = get_or_create_user(User_code),
    {ok, Resource_pid} = get_or_create_resource(Resource_name),
    ok = s_resource:publish(Resource_pid, Messages),
    {ok, <<"session_id_value">>}.

subscribe() %%% XXX
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_or_create_user(Code)->
    case Code of
	undefined -> User_code = s_utils:generate_code();
	Code -> User_code = Code
    end,
    case s_user:get_user(User_code) of
	{error, not_found} -> {ok, User} = s_user_sup:start_child(),
			      {ok, User};
	{ok, Pid} -> {ok, Pid}
    end.


get_or_create_resource(Name)->
    case s_resource:get_resource(Name) of
	{error, not_found} -> {ok, Resource} = s_resource_sup:start_child(Name),
			      {ok, Resource};
	{ok, Pid} -> {ok, Pid}
    end.

				      
