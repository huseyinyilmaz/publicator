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
    {Callback, Req1} = cowboy_req:qs_val(<<"callback">>, Req),

    case Callback of
        undefined ->
            {ok, Raw_data, Req2} = cowboy_req:body(Req1);
        _ ->
            {Raw_data, Req2} = cowboy_req:qs_val(<<"data">>, Req1)
    end,
    lager:debug("Session handler got request=~p", [Raw_data]),
    {Headers, Req3} = cowboy_req:headers(Req2),
    Request_data = jiffy:decode(Raw_data),
    {Request_plist} = Request_data,
    Auth_info = proplists:get_value(<<"auth_info">>, Request_plist),
    case publicator_core:create_consumer(Auth_info, Headers) of
        {ok, Consumer_code, _Consumer_pid} ->
            lager:info("Create consumer"),
            Body = jiffy:encode({[{<<"type">>, <<"session_created">>},
                                  {<<"data">>, Consumer_code}]});
        {error, permission_denied} ->
            Body = jiffy:encode({[{<<"error">>, <<"permission_denied">>}]})
    end,
    {ok, Req4} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                   h_utils:wrap_with_callback_fun(Callback, Body),
                   Req3),

    {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
        ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
