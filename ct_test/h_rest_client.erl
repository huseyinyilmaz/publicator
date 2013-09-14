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
-export([get_session/0, get_subscribtions/1, subscribe/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_request(Uri) ->
    Host = "http://localhost:8766/",
    Url = Host ++ Uri,
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	httpc:request(get, {Url, []}, [], []),
    Body.

post_request(Uri) ->
    Host = "http://localhost:8766/",
    Url = Host ++ Uri,
    {ok, {{_Version, 204, _ReasonPhrase}, _Headers, Body}} =
	httpc:request(post, {Url, [], "text/html", []}, [], [{body_format, string},
								    {relaxed, true}]),
    Body.

get_session()->
    Uri = "session/",
    Body = get_request(Uri),
    {[{<<"session">>,Session_id}]} = jiffy:decode(Body),
    Session_id.

get_subscribtions(Session_id) ->
    Uri = prefix_session_id(Session_id, "subscribtions/"),

    Body = get_request(Uri),
    jiffy:decode(Body).

subscribe(Session_id, Channel_code) ->
    Uri = prefix_session_id(Session_id,
			    "subscribtions/" ++ binary_to_list(Channel_code) ++ "/"),
    io:format("XXXXX~p~n",[Uri]),
    
    post_request(Uri).    

%%%===================================================================
%%% Internal functions
%%%===================================================================

prefix_session_id(Session_id, Uri) ->
    binary_to_list(Session_id) ++ "/" ++ Uri.
    
