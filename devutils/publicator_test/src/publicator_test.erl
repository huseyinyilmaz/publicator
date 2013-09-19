%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2013 by Huseyin Yilmaz <huseyin@saturn.local>
%%%-------------------------------------------------------------------
-module(publicator_test).

%% API
-export([main/1]).

%%%===================================================================
%%% API
%%%===================================================================
main([Host, Channel_count_str, Consumer_count_str]) ->
    try
	Channel_count = list_to_integer(Channel_count_str),
	Consumer_count = list_to_integer(Consumer_count_str),
	io:format("Host = ~p~n Channel_count = ~p~n Consumer_count = ~p~n",
		  [Host, Channel_count, Consumer_count])
    catch
        _:_ ->
            usage()
    end;
		
main(_) -> usage().
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
usage() ->
    io:format("USAGE: publicator_test host_name channel_count consumer_count").
