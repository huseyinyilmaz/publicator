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
-export([init/3, options/2, handle/2, terminate/3]).


%%%===================================================================
%%% API
%%%===================================================================


init(_Transport, Req, []) ->
        {ok, Req, undefined}.

%% disable same origin policy
options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    {ok, Req2, State}.

handle(Req, State) ->
    {ok, Raw_data, Req1} = cowboy_req:body(Req),
    {Headers, Req2} = cowboy_req:headers(Req1),
    Request_data = jiffy:decode(Raw_data),
    {Request_plist} = Request_data,
    Auth_info = proplists:get_value(<<"auth_info">>, Request_plist),
    case server:create_consumer(Auth_info, Headers) of
        {ok, Consumer_code, _Consumer_pid} ->
            Body = jiffy:encode({[{<<"session">>, Consumer_code}]});
        {error, permission_denied} ->
            Body = jiffy:encode({[{<<"error">>, <<"permission_denied">>}]})
    end,
    {Body, Req2, State}.

terminate(_Reason, _Req, _State) ->
        ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
