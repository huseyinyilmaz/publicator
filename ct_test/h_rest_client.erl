%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Http rest erlang client library for publicator
%%% @end
%%% Created : 14 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(h_rest_client).

%% API
-export([get_session/0, get_subscribtions/1, subscribe/2, publish/3]).
-export([get_messages/1]).
%%%===================================================================
%%% API
%%%===================================================================

-define(HOST, "http://127.0.0.1:8766/").
-define(OPTS, [{connect_timeout, 1000000},
               {socket_options, [%{keepalive, true},
                                 {active, false}]}]).
-define(TIMEOUT, infinity).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_request(Uri) ->
    Url = ?HOST ++ Uri,
    {ok, "200" , _Headers, Body} =
	ibrowse:send_req(Url, [], get,[],?OPTS, ?TIMEOUT),
    Body.

post_request(Uri, Data) ->
    Url = ?HOST ++ Uri,
    {ok, "204", _Headers, Body} =
	ibrowse:send_req(Url, [{"Content-Type", "text/html"}], post, Data, ?OPTS, ?TIMEOUT),
    Body.

get_session()->
    Uri = "session/",
    Body = get_request(Uri),
    {[{<<"session">>, Session_id}]} = jiffy:decode(Body),
    Session_id.

get_subscribtions(Session_id) ->
    Uri = prefix_session_id(Session_id, "subscribtions/"),

    Body = get_request(Uri),
    jiffy:decode(Body).

subscribe(Session_id, Channel_code) ->
    Uri = prefix_session_id(Session_id,
			    "subscribtions/" ++ binary_to_list(Channel_code) ++ "/"),
    io:format("XXXXX~p~n", [Uri]),
    
    post_request(Uri, []).    

publish(Session_id, Channel_code, Message) ->
    Uri = prefix_session_id(Session_id,
			    "messages/" ++ binary_to_list(Channel_code) ++ "/"),
    post_request(Uri, "message=" ++ Message).

get_messages(Session_id)->
    Uri = prefix_session_id(Session_id, "messages/"),
    get_request(Uri).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

prefix_session_id(Session_id, Uri) ->
    binary_to_list(Session_id) ++ "/" ++ Uri.
    
