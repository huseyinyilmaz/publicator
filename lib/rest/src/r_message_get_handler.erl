%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /subscribtions/:subscribtion url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(r_message_get_handler).

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
    case Channel_code of
	undefined ->
	    {ok, Result_text} = r_server_adapter:get_messages(Session_id),
	    error_logger:info_report({xxxxxxxxxxxxxxxxxx, Result_text}),
	    Body = jiffy:encode(Result_text);
	{_, Channel_code} -> Body = <<"Unknown method">> %% fix this
    end,
    
    {Body, Req3, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
