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
-export([get_session/2]).


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
      {{<<"text">>, <<"plain">>, '*'}, get_session},
      {{<<"text">>, <<"html">>, '*'}, get_session},
      {{<<"application">>, <<"json">>, '*'}, get_session},
      {{<<"application">>, <<"javascript">>, '*'}, get_session},
      {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, get_session}
     ], Req, State}.

get_session(Req, State)->
    {Callback, Req2} = cowboy_req:qs_val(<<"callback">>, Req),
    {Body, Req3, State} = get_json(Req2, State),
    case Callback of
	undefined -> {Body, Req3, State};
	_ -> {[Callback, <<"(">> , Body, <<");">>], Req3, State}
    end.


%% called for Get Request
get_json(Req, State) ->
    lager:debug("XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
    {Headers, Req2} = cowboy_req:headers(Req),
    lager:debug("XXXXXX=~p~n", [Headers]),
    {Auth_info, Req3} = cowboy_req:qs_val(<<"auth_info">>, Req2),
    lager:debug("YYYYYYYYYYYYYYYYYYYYYYYYYYY"),
    
    {ok, Consumer_code, _Consumer_pid} = server:create_consumer(Auth_info, Headers),
    Body = jiffy:encode({[{<<"session">>, Consumer_code}]}),
    {Body, Req3, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
