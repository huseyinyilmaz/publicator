%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc Rest interface application interface for publicator
%%%
%%% @end
%%% Created : 06 Jul 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(rest).

%% API
-export([start/0, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts rest server
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
        ok = application:start(rest).

%%--------------------------------------------------------------------
%% @doc
%% Stops rest server
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    ok = application:stop(rest),
    ok = application:stop(cowboy),
    ok = application:stop(ranch),
    ok = application:stop(crypto).


%%%===================================================================
%%% Internal functions
%%%===================================================================
