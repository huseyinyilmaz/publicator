
%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Debugging code 
%%% @end
%%% Created : 21 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(debug).

%% API
-export([init/0, start/0, stop/0, restart/0]).
%% -compile([{parse_transform, lager_transform}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @docn
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    io:format("Initializing environment", []),
    %% application:start(sasl),
    %% appmon:start(),
    code:add_patha("deps/lager/ebin"),
    code:add_patha("deps/ranch/ebin"),
    code:add_patha("deps/cowlib/ebin"),
    code:add_patha("deps/cowboy/ebin"),
    code:add_patha("deps/jiffy/ebin"),
    code:add_patha("deps/mimetypes/ebin"),
    code:add_patha("deps/gproc/ebin"),
    code:add_patha("deps/ibrowse/ebin"),

    code:add_patha("lib/http/ebin"),
    code:add_patha("lib/server/ebin"),
    lager:start(),
    application:start(gproc),    
    sync:go(),
    lager:error("test"),
    ok.

-spec start() -> ok.
start() ->
    io:format("Starting apps", []),
    ok = server:start(),
    ok = http:start(),
    ok.

-spec stop() -> ok.
stop() ->
    http:stop(),
    server:stop(),
    ok.

-spec restart() -> ok.
restart() ->
    stop(),
    start().

%%%===================================================================
%%% Internal functions
%%%===================================================================
