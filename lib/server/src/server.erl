%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>

-module(server).


%% API
-export([start/0, stop/0]).
-export([get_messages/3, publish/4]).

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
-spec get_messages(binary(), binary(),binary()) -> undefined.
get_messages(User_code, Category_name, Resource_name) ->
    User_pid = get_or_create_user(User_code),
    {ok, <<"session_id_value">>, [User_code,
				  Category_name,
				  Resource_name,
				  s_user:get_code(User_pid)]}.


publish(_User_code, _Category_name, _Resource_name, _Messages)->
    {ok, <<"session_id_value">>}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_or_create_user(Code)->
    User_code = case Code of
		    undefined ->
			s_utils:generate_code();
		    _  ->
			Code
		end,
    s_user:get_user(User_code).

				      
