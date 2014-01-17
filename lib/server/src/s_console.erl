%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Console interface for server application
%%% @end
%%% Created : 17 Jan 2014 by Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%%-------------------------------------------------------------------
-module(s_console).

%% API
-export([connect/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec connect([Node::list()]) -> ok.
connect([Node]) when is_list(Node)->
    pong = net_adm:ping(list_to_atom(Node)),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
