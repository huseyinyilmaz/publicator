%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Javascript http_transport handler.
%%% @end
%%% Created :  3 Feb 2014 by Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%%-------------------------------------------------------------------
-module(h_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% API

%% @doc POST echo handler.
init(_Transport, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    %% HasBody = cowboy_req:has_body(Req2),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    %{[{<<"type">>,<<"get_messages">>}, .....]}
    {Request_data} = jiffy:decode(Body),
    Request_type = proplists:get_value(<<"type">>, Request_data),
    {Headers, Req3} = cowboy_req:headers(Req2),
    Body = h_generic_handler:handle_request(Request_type, Session_id, Request_data, Headers),
    {ok, Req4} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                   Body, Req3),
    {ok, Req4, State}.



%% maybe_echo(<<"POST">>, true, Req) ->
%%         {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
%%         Echo = proplists:get_value(<<"echo">>, PostVals),
%%         echo(Echo, Req2);
%% maybe_echo(<<"POST">>, false, Req) ->
%%         cowboy_req:reply(400, [], <<"Missing body.">>, Req);
%% maybe_echo(_, _, Req) ->
%%         %% Method not allowed.
%%         cowboy_req:reply(405, Req).

%% echo(undefined, Req) ->
%%         cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
%% echo(Echo, Req) ->
%%         cowboy_req:reply(200, [
%%                 {<<"content-type">>, <<"text/plain; charset=utf-8">>}
%%         ], Echo, Req).

terminate(_Reason, _Req, _State) ->
        ok.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
