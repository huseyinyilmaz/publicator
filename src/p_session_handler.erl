%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(p_session_handler).

%% API
-export([init/3, handle/2, terminate/3]).

-include("../deps/publicator_core/include/publicator_core.hrl").

%%%===================================================================
%%% API
%%%===================================================================


init(_Transport, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
    {Callback, Req_callback} = cowboy_req:qs_val(<<"callback">>, Req),
    {Text, Req_text} = p_utils:get_request_text(Req_callback),
    lager:debug("RequestText=~p~n", [Text]),
    lager:debug("cowboy_req:has_body=~p~n", [cowboy_req:has_body(Req)]),
    #message{meta=Meta} = p_utils:parse_request_text(Text),
    lager:debug("======================== Debug Start ============", []),
    lager:debug("RequestMeta=~p~n", [Meta]),
    case publicator_core:create_producer(Meta) of
        {ok, Consumer_code, _Consumer_pid} ->
            lager:info("Create consumer"),
            Body = jiffy:encode(#{<<"type">> => <<"session_created">>,
                                  <<"data">> => Consumer_code});
        {error, permission_denied} ->
            Body = jiffy:encode(#{<<"error">> => <<"permission_denied">>})
    end,
    lager:debug("Response_body=~p~n", [Body]),
    {ok, Req_resp} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                   p_utils:wrap_with_callback_fun(Callback, Body),
                   Req_text),
    {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
        ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
