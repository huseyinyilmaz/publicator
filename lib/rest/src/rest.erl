%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc Rest interface application interface for huloo
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
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowboy),
        application:start(rest).

%%--------------------------------------------------------------------
%% @doc
%% Stops rest server
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    application:stop(rest),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto).


%%%===================================================================
%%% Internal functions
%%%===================================================================
