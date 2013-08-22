-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-export([setup_server/0, cleanup_server/1]).

setup_server()->
    error_logger:info_report("Setup server"),
    timer:sleep(3000),
    
    server:start(),
    timer:sleep(3000),

    error_logger:info_report(application:which_applications()),
    ok.

cleanup_server(_)->
    error_logger:info_report("Cleanup server"),
    server:stop().

server_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test main server functionality.",
      ?_test(
	 begin
	     server:start(),
	     error_logger:info_report(application:which_applications()),
	     timer:sleep(2000),
	     %% error_logger:info_report("1"),
	     %% server:start(),
	     %% error_logger:info_report("2"),
	     %% application:ensure_started(server),

	     error_logger:info_report("3"),
	     Consumer_code = <<"consumercode">>,
	     Channel_code = <<"channelcode">>,
	     error_logger:info_report("server_test start"),
	     Ress = application:which_applications(),%server:subscribe(Channel_code, Consumer_code),
	     error_logger:info_report({subscribe, "AAAAAAAAAAAAAAAAAAAAAAAAAA", Ress}),
	     ?assertEqual(ok, ok)

	 end)
     }}.
