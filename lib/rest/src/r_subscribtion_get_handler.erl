%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /subscribtions/:subscribtion url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(r_subscribtion_get_handler).

-export([init/3, allowed_methods/2,
	 content_types_provided/2]).
-export([get_json/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


%% GET
content_types_provided(Req, State) ->
    error_logger:info_report(content_types_provided),
	{[
	  {{<<"text">>, <<"plain">>, []}, get_json},
	  {{<<"text">>, <<"html">>, []}, get_json},
	  {{<<"application">>, <<"json">>, []}, get_json}
	], Req, State}.

%% called for Get Request
get_json(Req, State) ->
    {Session_id, Req1} = r_utils:get_or_create_session(Req),
    {Method, Req2} = cowboy_req:method(Req1),
    {Channel_code, Req3} = cowboy_req:binding(channel, Req2),
    
    error_logger:info_report({get_json, sessin_id, Session_id}),
    %% {ok, Response} = r_server_adapter:get_channels(),
    %% Body = jiffy:encode(Response),
    %% {Body, Req1, State}.
    case {Method, Channel_code} of
	{ <<"GET">>, undefined} ->
	    {ok, Result_text} = r_server_adapter:get_subscribtions(Session_id),
	    error_logger:info_report({xxxxxxxxxxxxxxxxxx, Result_text}),
	    Body = jiffy:encode(Result_text);
	{_, Channel_code} -> Body = <<"Unknown method">> %% fix this
    end,
    
    {Body, Req3, State}.

%% POST
%% content_types_accepted(Req, State)->
%%     error_logger:info_report(content_types_accepted),
%%     {[{{<<"text">>, <<"plain">>, []}, post_json},
%%       {{<<"text">>, <<"html">>, []}, post_json},
%%       {{<<"application">>, <<"json">>, []}, post_json}
%%      ], Req, State}.

%% post_json(Req, State) ->
%%     error_logger:info_report(post_json),
%%     {Session_id, Req1} = r_utils:get_or_create_session(Req),
%%     {Method, Req2} = cowboy_req:method(Req1),
%%     {Channel_code, Req3} = cowboy_req:binding(channel, Req2),
%%     case {Method, Channel_code} of
%% 	{ _, undefined} -> Result_status = false;
%% 	{<<"POST">>, Channel_code} ->
%% 	    ok = r_server_adapter:subscribe(Channel_code, Session_id),
%% 	    Result_status = true;
%% 	_ -> Result_status = false
%%     end,
%%     {Result_status, Req3, State}.

%% delete_resource(Req, State) ->
%%     error_logger:info_report(delete_resource),
%%     {Session_id, Req1} = r_utils:get_or_create_session(Req),
%%     {Channel_code, Req2} = cowboy_req:binding(channel, Req1),
%%     case Channel_code of
%% 	undefined -> Result_status = false;
%% 	Channel_code ->
%% 	    ok = r_server_adapter:unsubscribe(Channel_code, Session_id),
%% 	    Result_status = true
%%     end,
    
%%     {Result_status, Req2, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
