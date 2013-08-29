-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-export([setup_server/0, cleanup_server/1]).

-define(CONSUMER1, <<"consumercode1">>).
-define(CONSUMER2, <<"consumercode2">>).






-define(CHANNEL1, <<"channelcode1">>).
-define(CHANNEL2, <<"channelcode2">>).
-define(MESSAGE1, <<"message1">>).
-define(MESSAGE2, <<"message2">>).

setup_server()->
    error_logger:info_report("Setup server"),
    %% ok = application:start(sasl),
    ok = server:start().

cleanup_server(_)->
    error_logger:info_report("Cleanup server"),
    ok = server:stop().
    %% ok = application:stop(sasl).

server_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test subscribe unsubscribe functionality",
      ?_test(
	 begin
	     Consumer_code1 = ?CONSUMER1,
	     Consumer_code2 = ?CONSUMER2,
	     Channel_code = ?CHANNEL1,
             %% tests subscribe
             ?assertEqual(ok, server:subscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:subscribe(Consumer_code2, Channel_code)),
	     % make sure to wait until consumers register themselves
	     timer:sleep(20),
	     %% test get channels
             {ok,[Channel_code]} = server:get_channels(),
	     %% test get subscribtions
             {ok,[Channel_code]} = server:get_subscribtions(Consumer_code1),
	     {ok,[Channel_code]} = server:get_subscribtions(Consumer_code2),
	     %% test send message
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE1),
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE2),


	     %% ok = server:publish(Consumer_code2, Channel_code, ?MESSAGE2),
	     
             %% tests unsubscribe
             ?assertEqual(ok, server:unsubscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:unsubscribe(Consumer_code2, Channel_code)),
	     % make sure to wait until consumers unregister themselves
	     timer:sleep(20),
             %% error_logger:info_report({"debug", server:get_channels()}),
             {ok,[Channel_code]} = server:get_channels(),
             {ok,[]} = server:get_subscribtions(Consumer_code1),
             {ok,[]} = server:get_subscribtions(Consumer_code2),
             ok
	 end)
     }}.

%% messagging_test_() ->
%%     {setup,
%%      fun setup_server/0,
%%      fun cleanup_server/1,
%%      {"Test messaging functionality",
%%       ?_test(
%% 	 begin
%%        ok
%% 	 end)
%%      }}.
