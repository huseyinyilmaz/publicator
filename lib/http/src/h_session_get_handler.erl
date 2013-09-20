%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(h_session_get_handler).

%% API
-export([init/3, allowed_methods/2,
	 content_types_provided/2]).
-export([get_json/2]).

%%%===================================================================
%%% API
%%%===================================================================


init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

%% GET
content_types_provided(Req, State) ->
    {[
      {{<<"text">>, <<"plain">>, '*'}, get_json},
      {{<<"text">>, <<"html">>, '*'}, get_json},
      {{<<"application">>, <<"json">>, '*'}, get_json},
      {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, get_json}
     ], Req, State}.

%% called for Get Request
get_json(Req, State) ->
    {ok, Consumer_code, _Consumer_pid} = h_server_adapter:create_consumer(),
    Body = jiffy:encode({[{<<"session">>, Consumer_code}]}),
    {Body, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
