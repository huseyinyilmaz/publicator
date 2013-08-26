%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Basic tests for development. until got tests working.
%%% @end
%%% Created : 26 Aug 2013 by Huseyin Yilmaz <huseyin@saturn.local>
%%%-------------------------------------------------------------------
-module(tests).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================
setup_server()->
    error_logger:info_report("Setup server"),
    server:start().

cleanup_server()->
    error_logger:info_report("Cleanup server"),
    server:stop().

test_resources()->
    Consumer_code1 = <<"consumercode1">>,
    Consumer_code2 = <<"consumercode2">>,
    Channel_code = <<"channelcode">>,
    %% tests subscribe
    ok = server:subscribe(Channel_code, Consumer_code1),
    ok = server:subscribe(Channel_code, Consumer_code2),
    {ok,[Channel_code]} = server:get_channels(),
    {ok,[Channel_code]} = server:get_subscribtions(Consumer_code1),
    {ok,[Channel_code]} = server:get_subscribtions(Consumer_code2),

    %% tests unsubscribe
    ok = server:unsubscribe(Channel_code, Consumer_code1),
    ok = server:subscribe(Channel_code, Consumer_code2),
    error_logger:info_report({"debug", server:get_channels()}),
    {ok,[Channel_code]} = server:get_channels(),
    {ok,[]} = server:get_subscribtions(Consumer_code1),
    {ok,[]} = server:get_subscribtions(Consumer_code2).

start()->
    setup_server(),
    test_resources(),
    cleanup_server().

%%%===================================================================
%%% Internal functions
%%%===================================================================
