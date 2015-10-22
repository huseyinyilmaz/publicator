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
    lager:info("subscribe producer_code=~p, channel_code=~p, meta=~p~n ",
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
    end.

handle_request(<<"unsubscribe">>, Session_id, Data, _Extra_data)->
    ok = publicator_core:unsubscribe(Session_id, Data),
    ok = publicator_core:remove_message_handler(Session_id, self()),
    p_utils:make_response(<<"unsubscribed">>, Data);

handle_request(<<"get_subscribtions">>, Session_id, _Data, _Extra_data)->
    case publicator_core:get_subscribtions(Session_id) of
        {ok, Subscribtion_data} -> p_utils:make_response(<<"subscribtions">>, Subscribtion_data);
        {error, producer_not_found} -> p_utils:no_session_response()
    end;


handle_request(<<"get_consumers">>, Session_id, Data, Extra_data)->
    {Data_plist} = Data,
    {Get_consumers_data} = proplists:get_value(<<"data">>, Data_plist),
    Channel_code = proplists:get_value(<<"channel_code">>, Get_consumers_data),
    case publicator_core:get_consumers(Session_id, Channel_code, Extra_data) of
        {error, producer_not_found} ->
            p_utils:no_session_response();
        {error, permission_denied} ->
            p_utils:permission_denied_response();
        {error, invalid_channel_code} ->
            p_utils:invalid_channel_code_response();
        {ok, Consumer_list} when is_list(Consumer_list)->
            p_utils:make_response(<<"consumers">>, Consumer_list,
                                  [{<<"channel_code">>, Channel_code}])
    end;

handle_request(<<"publish">>, Session_id, Data, Extra_data)->
    {Data_plist} = Data,
    {Subscribe_data} = proplists:get_value(<<"data">>, Data_plist),
    Channel_code = proplists:get_value(<<"channel_code">>, Subscribe_data),
    Message = proplists:get_value(<<"message">>, Subscribe_data),
    lager:debug("Publishing a message \"~p\" on channel ~p by session ~p",
               [Message, Channel_code, Session_id]),
    case publicator_core:publish(Session_id, Channel_code, Message, Extra_data) of
        ok -> p_utils:ok_response();
        {error, producer_not_found} ->
            p_utils:no_session_response();
        {error, permission_denied} ->
            p_utils:permission_denied_response()
    end;

handle_request(Type, Session_id, Data, _Extra_data)->
    p_utils:make_response(<<"unhandled_msg">>, Data,
                          [{<<"request_type">>, Type},
                           {<<"session">>, Session_id}]).




%%%===================================================================
%%% Internal functions
%%%===================================================================
