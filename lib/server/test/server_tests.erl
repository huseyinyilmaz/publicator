-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-export([setup_server/0, cleanup_server/1]).

setup_server()->
    error_logger:info_report("Setup server"),
    server:start().

cleanup_server(_)->
    server:stop().

server_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test main server functionality.",
      ?_test(
	 begin
	     application:ensure_started(server),
	     Consumer_code = <<"consumercode">>,
	     Channel_code = <<"channelcode">>,
	     error_logger:info_report("server_test start"),
	     timer:sleep(5000),
	     Ress = server:subscribe(Channel_code, Consumer_code),
	     error_logger:info_report({subscribe, "AAAAAAAAAAAAAAAAAAAAAAAAAA", Ress}),
	     ?assertEqual(ok, ok)

	 end)
     }}.
