%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /channels/ url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(r_channel_handler).

-export([init/3]).
-export([allowed_methods/2
	 %% ,content_types_accepted/2
	 ,content_types_provided/2
	]).

-export([%% process_json/2,
	 handle_json/2]).


init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

%% processes given request.
%% content_types_accepted(Req, State)->
%%     error_logger:info_report(content_types_accepted),
%%     {[{{<<"text">>, <<"plain">>, []}, process_json},
%%       {{<<"*">>, <<"*">>, []}, process_json}

%%      ], Req, State}.


%% creates response. gets Req and State from c_t_accepted method
content_types_provided(Req, State) ->
    error_logger:info_report(content_types_provided),
	{[
	  {{<<"text">>, <<"plain">>, []}, handle_json},
	  {{<<"text">>, <<"html">>, []}, handle_json},
	  {{<<"application">>, <<"json">>, []}, handle_json}
	], Req, State}.

%% called for post
%% process_json(Req, State) ->
%%     error_logger:info_report({process_json, Req, State}),
%%     %% {ok, Req1} = r_server_adapter:subscribe(Req),
%%     St = [1,2,3,4],
%%     {true, Req, St}.

%% called for Get Request
handle_json(Req, State) ->
    {Session_id, Req1} = r_utils:get_or_create_session(Req), 
    error_logger:info_report({handle_json, sessin_id, Session_id}),
    {ok, Response} = r_server_adapter:get_channels(),
    Body = jiffy:encode(Response),
    {Body, Req1, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
