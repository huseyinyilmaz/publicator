%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /subscribtions/:subscribtion url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(h_subscribtion_handler).

-export([init/3, allowed_methods/2, content_types_accepted/2, delete_resource/2]).
-export([post_json/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"DELETE">>], Req, State}.

%% POST
content_types_accepted(Req, State)->
    {[{{<<"text">>, <<"plain">>, []}, post_json},
      {{<<"text">>, <<"html">>, []}, post_json},
      {{<<"application">>, <<"json">>, []}, post_json}
     ], Req, State}.

post_json(Req, State) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    {Method, Req2} = cowboy_req:method(Req1),
    {Channel_code, Req3} = cowboy_req:binding(channel, Req2),
    case {Method, Channel_code} of
	{ _, undefined} -> Result_status = false;
	{<<"POST">>, Channel_code} ->
	    ok = h_server_adapter:subscribe(Session_id, Channel_code),
	    Result_status = true;
	_ -> Result_status = false
    end,
    {Result_status, Req3, State}.

delete_resource(Req, State) ->
    {Session_id, Req1} = h_utils:get_or_create_session(Req),
    {Channel_code, Req2} = cowboy_req:binding(channel, Req1),
    case Channel_code of
	undefined -> Result_status = false;
	Channel_code ->
	    ok = h_server_adapter:unsubscribe(Session_id, Channel_code),
	    Result_status = true
    end,
    
    {Result_status, Req2, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
