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

init(_Transport, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req),

    {Callback, Req2} = cowboy_req:qs_val(<<"callback">>, Req1),
    case Callback of
        undefined ->
            {ok, Raw_data, Req3} = cowboy_req:body(Req2);
        _ ->
            {Raw_data, Req3} = cowboy_req:qs_val(<<"data">>, Req2)
    end,

    Request_data = jiffy:decode(Raw_data),
    {Request_plist} = Request_data,
    Request_type = proplists:get_value(<<"type">>, Request_plist),

    {Headers, Req4} = cowboy_req:headers(Req3),
    lager:debug("Http interface got data ~p", [Request_data]),
    Body = h_generic_handler:handle_request(Request_type, Session_id, Request_data, Headers),
    {ok, Req5} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                   h_utils:wrap_with_callback_fun(Callback, Body),
                   Req4),
    {ok, Req5, State}.

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
