%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /subscribtions/:subscribtion url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(r_message_handler).

-export([init/3, allowed_methods/2, content_types_accepted/2, delete_resource/2]).
-export([post_json/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% POST
content_types_accepted(Req, State)->
    error_logger:info_report(content_types_accepted),
    {[{{<<"text">>, <<"plain">>, []}, post_json},
      {{<<"text">>, <<"html">>, []}, post_json},
      {{<<"application">>, <<"json">>, []}, post_json}
     ], Req, State}.

post_json(Req, State) ->
    error_logger:info_report(post_json),
    {Session_id, Req1} = r_utils:get_or_create_session(Req),
    {Channel_code, Req2} = cowboy_req:binding(channel, Req1),
    {ok, [{<<"message">>, Message}], Req3} = cowboy_req:body_qs(Req2),
    error_logger:info_report({post_json, Session_id, Message}),
    ok = r_server_adapter:publish(Session_id, Channel_code, Message),
    {true, Req3, State}.

delete_resource(Req, State) ->
    error_logger:info_report(delete_resource),
    {Session_id, Req1} = r_utils:get_or_create_session(Req),
    {Channel_code, Req2} = cowboy_req:binding(channel, Req1),
    case Channel_code of
	undefined -> Result_status = false;
	Channel_code ->
	    ok = r_server_adapter:unsubscribe(Channel_code, Session_id),
	    Result_status = true
    end,
    
    {Result_status, Req2, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
