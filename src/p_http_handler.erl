%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Javascript http_transport handler.
%%% @end
%%% Created :  3 Feb 2014 by Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%%-------------------------------------------------------------------
-module(p_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
%% API

init(_Transport, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
    lager:debug("======================== Debug Start ============", []),
    {Session_id, Req_session} = cowboy_req:binding(session, Req),
    {Callback, Req_callback} = cowboy_req:qs_val(<<"callback">>, Req_session),
    {Text, Req_text} = p_utils:get_request_text(Req_callback),
    lager:debug("RequestText=~p~n", [Text]),
    Msg = p_utils:parse_request_text(Text, Session_id),
    lager:debug("Message=~p~n", [Msg]),
    Body = p_generic_handler:handle_request(Msg),
    {ok, Req5} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                   p_utils:wrap_with_callback_fun(Callback, Body),
                   Req_text),
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
