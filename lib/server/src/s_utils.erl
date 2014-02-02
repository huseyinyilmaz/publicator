%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 06 Jul 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(s_utils).

%% API
-export([generate_code/0, get_env/3, set_env/3, ensure_started/1]).
-export([get_channel_cache_size/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates random code as binary
%% @end
%%--------------------------------------------------------------------
-spec generate_code() -> binary().
generate_code()->
    list_to_binary(
      string:to_lower(
	integer_to_list(
	  erlang:phash2({node(), now()}), 36))).

%%--------------------------------------------------------------------
%% @doc
%% gets configuration for given app from sys.config file.
%% if configuration does not exists, it uses given default value.
%% @end
%%--------------------------------------------------------------------
-spec get_env(atom(), atom(), any()) -> any().
get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.

%%--------------------------------------------------------------------
%% @doc
%% Set environment variable for given app
%% @end
%%--------------------------------------------------------------------
-spec set_env(atom(), atom(), any()) -> any().
set_env(AppName, Key, Value) ->
    application:set_env(AppName, Key, Value).


-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

get_channel_cache_size(Channel_code)->
    Channel_cache_list = get_env(server, channel_cache, [{all, 0}]),
    Count = choose_channel_cache(Channel_code,Channel_cache_list),
    lager:info("================================================="),
    lager:info("Chosen count for ~p is ~p", [Channel_code, Count]),
    Count.
%%%===================================================================
%%% Internal functions
%%%===================================================================


choose_channel_cache(Channel_code,[{Name,Count}|Channel_cache_list]) ->
    case Name of
        Channel_code -> Count;
        all -> Count;
        _ ->
            choose_channel_cache(Channel_code,Channel_cache_list)
    end.
            
