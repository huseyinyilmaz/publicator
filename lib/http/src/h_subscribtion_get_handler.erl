%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /subscribtions/:subscribtion url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(h_subscribtion_get_handler).

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
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    error_logger:info_report({get_json, sessin_id, Session_id}),
    case h_server_adapter:get_subscribtions(Session_id) of
	{ok, Result_text} ->
	    Body = jiffy:encode({dict:to_list(Result_text)}),
	    error_logger:info_report({subscribtions, get, Body});
	{error, consumer_not_found} ->
	    Body = h_utils:no_session_response(),
	    error_logger:info_report({subscribtions, get, <<"no session found">>})
    end,
    {Body, Req1, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
