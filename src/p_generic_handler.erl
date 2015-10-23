%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(p_generic_handler).

%% API
-export([handle_request/1]).

-include("../deps/publicator_core/include/publicator_core.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_request(#message{type= <<"get_messages">>,
                        producer_code=Producer_code})->
    case publicator_core:get_messages(Producer_code) of
        {ok, Messages_list} ->
            p_utils:message_list_response(Messages_list);
        {error, producer_not_found} ->
            p_utils:no_session_response()
    end;

handle_request(#message{type= <<"subscribe">>,
                        producer_code=Producer_code,
                        channel_code=Channel_code,
                        meta=Meta})->
    lager:info("Subscribe producer_code=~p, channel_code=~p, meta=~p~n ",
               [Producer_code, Channel_code, Meta]),
    case publicator_core:subscribe(Producer_code, Channel_code, Meta) of
        {error, invalid_channel_code} ->
            p_utils:error_response(<<"invalid_channel_code">>);
        {error, producer_not_found} ->
            p_utils:no_session_response();
        {error, permission_denied} ->
            p_utils:permission_denied_response();
        ok->
            p_utils:make_response(<<"subscribed">>, Channel_code)
    end;

handle_request(#message{type= <<"unsubscribe">>,
                        producer_code=Producer_code,
                        channel_code=Channel_code})->
    ok = publicator_core:unsubscribe(Producer_code, Channel_code),
    ok = publicator_core:remove_message_handler(Producer_code, self()),
    p_utils:make_response(<<"unsubscribed">>, Channel_code);

handle_request(#message{type= <<"get_subscribtions">>,
                        producer_code=Producer_code})->

    case publicator_core:get_subscribtions(Producer_code) of
        {ok, Subscribtion_data} -> p_utils:make_response(<<"subscribtions">>,
                                                         Subscribtion_data);
        {error, producer_not_found} -> p_utils:no_session_response()
    end;

handle_request(#message{type= <<"publish">>,
                        producer_code=Producer_code,
                        channel_code=Channel_code,
                        data=Data,
                        meta=Meta})->
    Msg = publicator_core:make_message(Producer_code, Channel_code, Data, Meta),
    case publicator_core:publish(Msg) of
        ok -> p_utils:ok_response();
        {error, producer_not_found} ->
            p_utils:no_session_response();
        {error, permission_denied} ->
            p_utils:permission_denied_response()
    end;

handle_request(Msg)->
    p_utils:make_response(<<"unhandled_msg">>, Msg).




%%%===================================================================
%%% Internal functions
%%%===================================================================
