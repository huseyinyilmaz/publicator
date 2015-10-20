%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%% assertion utilities utilities for common test.
%%% @end
%%% Created : 16 Oct 2015 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(ctcheck).

%% API
-export([check/4]).
-export([equal/2]).
-export([not_equal/2]).
-export([is_pid/1]).


%%%===================================================================
%%% API
%%%===================================================================

check(true, _Message, _Args, _Return) -> true;
check(false, Message, Args, Return) ->
    ct:pal(error, Message, Args),
    Return.


equal(First, Second) ->
    check(First == Second,
          "Assertion Failed:~nvalue=~p~nexpected=~p.", [First,Second],
          ctcheck_equal_failed).


not_equal(First, Second) ->
    check(First =/= Second,
          "Assertion Failed:~nvalue ~p is equal to ~p.", [First, Second],
          ctcheck_not_equal_failed).

is_pid(Pid) ->
    check(erlang:is_pid(Pid),
          "Assertion Failed:~nvalue ~p is not a pid.", [Pid],
          ctcheck_is_pid_failed).


%%%===================================================================
%%% Internal functions
%%%===================================================================
