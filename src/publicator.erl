%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc Rest interface application interface for publicator
%%%
%%% @end
%%% Created : 06 Jul 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(publicator).

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
    ok = pc_utils:ensure_started(ibrowse),
    ok = pc_utils:ensure_started(compiler),
    ok = pc_utils:ensure_started(syntax_tools),
    ok = pc_utils:ensure_started(goldrush),
    ok = pc_utils:ensure_started(lager),
    ok = pc_utils:ensure_started(crypto),
    ok = pc_utils:ensure_started(ranch),
    ok = pc_utils:ensure_started(cowlib),
    ok = pc_utils:ensure_started(cowboy),
    ok = pc_utils:ensure_started(jiffy),
    ok = pc_utils:ensure_started(publicator_core),
    ok = pc_utils:ensure_started(publicator).

%%--------------------------------------------------------------------
%% @doc
%% Stops rest server
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    ok = application:stop(cowboy),
    ok = application:stop(cowlib),
    ok = application:stop(ranch),
    ok = application:stop(jiffy),
    ok = application:stop(crypto),
    ok = application:stop(publicator_core),
    ok = application:stop(publicator),
    ok = application:stop(lager),
    ok = application:stop(goldrush),
    ok = application:stop(syntax_tools),
    ok = pc_utils:ensure_started(compiler).


%%%===================================================================
%%% Internal functions
%%%===================================================================
