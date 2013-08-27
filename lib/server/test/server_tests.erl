-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-export([setup_server/0, cleanup_server/1]).

setup_server()->
    error_logger:info_report("Setup server"),
    ok = application:start(sasl),
    ok = server:start().

cleanup_server(_)->
    error_logger:info_report("Cleanup server"),
    ok = server:stop(),
    ok = application:stop(sasl).

server_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test subscribe unsubscribe functionality",
      ?_test(
	 begin
	     Consumer_code1 = <<"consumercode1">>,
	     Consumer_code2 = <<"consumercode2">>,
	     Channel_code = <<"channelcode">>,
             %% tests subscribe
             ?assertEqual(ok, server:subscribe(Channel_code, Consumer_code1)),
             ?assertEqual(ok, server:subscribe(Channel_code, Consumer_code2)),
             %% {ok,[Channel_code]} = server:get_channels(),
             %% {ok,[Channel_code]} = server:get_subscribtions(Consumer_code1),
             %% {ok,[Channel_code]} = server:get_subscribtions(Consumer_code2),
         
             %% tests unsubscribe
             ?assertEqual(ok, server:unsubscribe(Channel_code, Consumer_code1)),
             ?assertEqual(ok, server:unsubscribe(Channel_code, Consumer_code2)),
             %% error_logger:info_report({"debug", server:get_channels()}),
             %% {ok,[Channel_code]} = server:get_channels(),
             %% {ok,[]} = server:get_subscribtions(Consumer_code1),
             %% {ok,[]} = server:get_subscribtions(Consumer_code2),
             ok
	     
	 end)
     }}.
