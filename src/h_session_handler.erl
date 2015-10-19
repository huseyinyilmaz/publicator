%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(h_session_handler).

%% API
-export([init/3, handle/2, terminate/3]).


%%%===================================================================
%%% API
%%%===================================================================


init(_Transport, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
    {Meta, Req_meta} = h_utils:get_meta(Req),
    lager:debug("======================== Debug Start ============", []),
    lager:debug("Get meta=~p~n", [Meta]),
    case publicator_core:create_producer(Meta) of
        {ok, Consumer_code, _Consumer_pid} ->
            lager:info("Create consumer"),
            Body = jiffy:encode(#{<<"type">> => <<"session_created">>,
                                  <<"data">> => Consumer_code});
        {error, permission_denied} ->
            Body = jiffy:encode(#{<<"error">> => <<"permission_denied">>})
    end,
    lager:debug("Response_body=~p~n", [Body]),
    Callback = maps:get(<<"callback">>, Meta, undefined),
    {ok, Req_resp} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                   h_utils:wrap_with_callback_fun(Callback, Body),
                   Req_meta),
    {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
        ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
