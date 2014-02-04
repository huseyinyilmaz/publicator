%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(h_generic_handler).

%% API
-export([handle_request/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_request(<<"get_messages">>, Session_id, _Data, _Extra_data)->
    case server:get_messages(Session_id) of
        {ok, Result_dict} ->
            %% Msg_list = [{<<"channel1">>, [{message, <<"msg">>}, ..]},
            %%             {<<"channel2">>,..}]

            %% Msg_json = [ {[{<<"channel1">>, [ {[{message, <<"meg">>}]},.. ]}]},
            %%                {<<"channel2">>, ....  }]}]
            Message_list = dict:to_list(Result_dict),
            Result_list = [{lists:map(
                              fun({Channel_name, Msg_list})->
                                      {Channel_name, lists:map(fun(S)->{[S]} end, Msg_list)}
                              end,
                              Message_list)}],
            jiffy:encode(Result_list);
        {error, consumer_not_found} ->
            h_utils:no_session_response()
    end;

handle_request(<<"subscribe">>, Session_id, Data, Extra_data)->
    {Data_plist} = Data,
    {Subscribe_data} = proplists:get_value(<<"data">>, Data_plist),
    Channel_code = proplists:get_value(<<"channel_code">>, Subscribe_data),
    Handler_type = case proplists:get_value(<<"type">>, Subscribe_data) of
                       <<"message_only">> -> message_only;
                       <<"all">> -> all
                   end,
    case server:subscribe(Session_id, Channel_code, Handler_type, Extra_data) of
        {error, invalid_channel_code} ->
            h_utils:error_response(<<"error">>, <<"invalid_channel_code">>);
        {error, consumer_not_found} ->
            h_utils:no_session_response();
        {error, permission_denied} ->
            h_utils:permission_denied_response();
        ok->
            h_utils:make_response(<<"subscribed">>, Channel_code)
    end;

handle_request(<<"unsubscribe">>, Session_id, Data, _Extra_data)->
    ok = server:unsubscribe(Session_id, Data),
    ok = server:remove_message_handler(Session_id, self()),
    h_utils:make_response(<<"unsubscribed">>, Data);

handle_request(<<"get_subscribtions">>, Session_id, _Data, _Extra_data)->
    {ok, Subscribtion_data} = server:get_subscribtions(Session_id),
    h_utils:make_response(<<"subscribtions">>, Subscribtion_data);

handle_request(<<"publish">>, Session_id, Data, Extra_data)->
    {Data_plist} = Data,
    {Subscribe_data} = proplists:get_value(<<"data">>, Data_plist),
    Channel_code = proplists:get_value(<<"channel_code">>, Subscribe_data),
    Message = proplists:get_value(<<"message">>, Subscribe_data),
    case server:publish(Session_id, Channel_code, Message, Extra_data) of
        ok -> h_utils:ok_response();
        {error, consumer_not_found} ->
            h_utils:no_session_response();
        {error, permission_denied} ->
            h_utils:permission_denied_response()
    end;

handle_request(Type, Session_id, Data, _Extra_data)->
    h_utils:make_response(<<"unhandled_msg">>, Data,
                          [{<<"request_type">>, Type},
                           {<<"session">>, Session_id}]).




%%%===================================================================
%%% Internal functions
%%%===================================================================
