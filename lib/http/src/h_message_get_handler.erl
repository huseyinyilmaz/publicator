%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Module that handles /subscribtions/:subscribtion url
%%% @end
%%% Created : 12 Jul 2013 by Huseyin Yilmaz <huseyin@saturn.local>

-module(h_message_get_handler).

-export([init/3, allowed_methods/2,
	 content_types_provided/2]).
-export([get_json/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


%% GET
content_types_provided(Req, State) ->
    lager:info(content_types_provided),
	{[
	  {{<<"text">>, <<"plain">>, []}, get_json},
	  {{<<"text">>, <<"html">>, []}, get_json},
	  {{<<"application">>, <<"json">>, []}, get_json}
	], Req, State}.

%% called for Get Request
get_json(Req, State) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    case Session_id of
        undefined -> Body = h_utils:error_response(<<"There is no session provided">>);
        Session_id ->
            case server:get_messages(Session_id) of
                {ok, Result_dict} ->
                    Body = msg_dict_to_json(Result_dict);
                {error, consumer_not_found} ->
                    Body = h_utils:no_session_response()
            end
    end,
    {Body, Req1, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

normalize_channel({Channel_name, Message_list})->
    {Channel_name, lists:map(fun(S)->{[S]} end, Message_list)}.


msg_dict_to_json(Message_dict)->
    %% Msg_list = [{<<"channel1">>, [{message, <<"msg">>}, ..]},
    %%             {<<"channel2">>,..}]

    %% Msg_json = [ {[{<<"channel1">>, [ {[{message, <<"meg">>}]},.. ]}]},
    %%                {<<"channel2">>, ....  }]}]
    Message_list = dict:to_list(Message_dict),
    lager:info("Message_list = ~p~n", [Message_list]),
    Result_list = [{lists:map(fun normalize_channel/1, Message_list)}],
    jiffy:encode(Result_list).
