-module(server_tests).
-include_lib("eunit/include/eunit.hrl").


setup_server()->
    error_logger:info_report(aaaaaaaaaaaaaaaaa),
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
	     Consumer_code = <<"consumercode">>,
	     Channel_code = <<"channelcode">>,
	     error_logger:info_report({bbbbbbbbbbbbbbbbb}),
	     Ress = server:subscribe(Channel_code, Consumer_code),
	     error_logger:info_report({subscribe, "AAAAAAAAAAAAAAAAAAAAAAAAAA", Ress}),
	     ?assertEqual(ok, no)

	 end)
     }}.
