%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /subscribtions/:subscribtion url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(r_message_handler).

-export([init/3]).
-export([allowed_methods/2, known_methods/2
	 ,content_types_accepted/2
	]).
-export([process_text/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

known_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.


%% processes given request.
content_types_accepted(Req, State)->
    error_logger:info_report(content_types_accepted),
    {[{{<<"text">>, <<"plain">>, []}, process_text}], Req, State}.


%% creates response. gets Req and State from c_t_accepted method
%% content_types_provided(Req, State) ->
%%     error_logger:info_report(content_types_provided),
%% 	{[
%% 	  {{<<"text">>, <<"plain">>, []}, handle_json},
%% 	  {{<<"text">>, <<"html">>, []}, handle_json},
%% 	  {{<<"application">>, <<"json">>, []}, handle_json}
%% 	], Req, State}.

process_text(Req, State) ->
    error_logger:info_report(process_json),
    {ok, Req1} = r_server_adapter:subscribe(Req),
    {true, Req1, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
