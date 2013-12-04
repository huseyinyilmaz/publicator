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
    {Channel_code, Req2} = cowboy_req:binding(channel, Req1),
    {ok, Qs, Req3} = cowboy_req:body_qs(Req2),
    Message_type = proplists:get_value(<<"message_type">>,Qs, message_only),
    case h_server_adapter:subscribe(Session_id, Channel_code, Message_type) of
	ok -> Resp = true;
	{error, consumer_not_found} ->
	    %% will return 422
	    Resp=false
    end,
    {Resp, Req3, State}.

delete_resource(Req, State) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    {Channel_code, Req2} = cowboy_req:binding(channel, Req1),
    case h_server_adapter:unsubscribe(Session_id, Channel_code) of
	ok -> Resp = true;
	{error, consumer_not_found} ->
	    %% this will return 500. it should return something else?
	    lager:info({subscribtions,delete,consumer_not_found}),
	    Resp=false
    end,
    {Resp, Req2, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
