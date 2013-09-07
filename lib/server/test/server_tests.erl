-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-export([setup_server/0, cleanup_server/1]).

-define(CONSUMER1, <<"consumercode1">>).
-define(CONSUMER2, <<"consumercode2">>).
-define(CHANNEL1, <<"channelcode1">>).
-define(CHANNEL2, <<"channelcode2">>).
-define(MESSAGE1, <<"message1">>).
-define(MESSAGE2, <<"message2">>).
-define(DELAY, 100).

setup_server()->
    error_logger:info_report("Setup server"),
    %% ok = application:start(sasl),
    ok = server:start().

cleanup_server(_)->
    error_logger:info_report("Cleanup server"),
    ok = server:stop().
    %% ok = application:stop(sasl).

server_uninitialized_session_test_()->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test uninitialized session functionality",
      ?_test(
	 begin
	     Consumer_code = ?CONSUMER1,
	     Channel_code = ?CHANNEL1,
	     %% test uninitialized sesssions
             ?assertEqual({error, consumer_not_found},
			  server:get_messages(Consumer_code, Channel_code)),
             ?assertEqual({error, consumer_not_found},
			  server:get_messages(Consumer_code)),
             ?assertEqual({error, consumer_not_found},
			  server:publish(Consumer_code, Channel_code, ?MESSAGE1)),
	     ?assertEqual({error, consumer_not_found},
			  server:subscribe(Consumer_code, Channel_code)),
	     ?assertEqual({error, consumer_not_found},
			  server:unsubscribe(Consumer_code, Channel_code)),
             ?assertEqual({error, consumer_not_found},
			  server:get_subscribtions(Consumer_code)),
             ok
	 end)
     }}.


server_subscribtion_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test subscribe unsubscribe functionality",
      ?_test(
	 begin
	     {ok, Consumer_code1, _} = server:create_consumer(),
	     {ok, Consumer_code2, _} = server:create_consumer(),
	     Channel_code = ?CHANNEL1,
	     Channel_code2 = ?CHANNEL2,
             %% tests subscribe
	     %% multiple subscribtions to same Channel should not create multiple
	     %% channel event handlers
             ?assertEqual(ok, server:subscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:subscribe(Consumer_code1, Channel_code2)),
             ?assertEqual(ok, server:subscribe(Consumer_code2, Channel_code)),

	     % make sure to wait until consumers register themselves
	     timer:sleep(?DELAY),
	     %% test get channels
             ?assertEqual({ok,[Channel_code2, Channel_code]}, server:get_channels()),
	     %% test get subscribtions
             ?assertEqual({ok,[Channel_code2, Channel_code]},
			  server:get_subscribtions(Consumer_code1)),
	     ?assertEqual({ok,[Channel_code]},
			  server:get_subscribtions(Consumer_code2)),
             %% tests unsubscribe
             ?assertEqual(ok, server:unsubscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:unsubscribe(Consumer_code2, Channel_code)),
	     % make sure to wait until consumers unregister themselves
	     timer:sleep(?DELAY),
             ?assertEqual({ok,[Channel_code2, Channel_code]}, server:get_channels()),
             ?assertEqual({ok,[Channel_code2]}, server:get_subscribtions(Consumer_code1)),
             ?assertEqual({ok,[]}, server:get_subscribtions(Consumer_code2)),
             ?assertEqual(ok, server:unsubscribe(Consumer_code2, Channel_code2)),
             ?assertEqual({ok,[]}, server:get_subscribtions(Consumer_code2)),
             ok
	 end)
     }}.
		 
server_message_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"Test publish get_message functionality",
      ?_test(
	 begin
	     {ok, Consumer_code1, _} = server:create_consumer(),
	     {ok, Consumer_code2, _} = server:create_consumer(),
	     Channel_code = ?CHANNEL1,
	     Channel_code2 = ?CHANNEL2,
	     timer:sleep(?DELAY),
             %% tests subscribe
	     %% multiple subscribtions to same Channel should not create multiple
	     %% channel event handlers
             ?assertEqual(ok, server:subscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:subscribe(Consumer_code2, Channel_code)),
	     % make sure to wait until consumers register themselves
	     timer:sleep(?DELAY),
	     %% test send message
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE1),
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE2),
	     timer:sleep(?DELAY),
	     %% test get_messages
	     {ok, Messages} = server:get_messages(Consumer_code2),
	     ?assertEqual({ok,[?MESSAGE1, ?MESSAGE2]}, dict:find(Channel_code, Messages)),
	     %% make sure that Messages has been cleared
	     {ok, Messages2} = server:get_messages(Consumer_code2),
	     ?assertEqual(error, dict:find(Channel_code, Messages2)),
	     %% make usre that original sender did not get the messages
	     {ok, Messages3} = server:get_messages(Consumer_code1),
	     ?assertEqual(error, dict:find(Channel_code, Messages3)),
	     %% make sure that channels are seperate
             ?assertEqual(ok, server:subscribe(Consumer_code2, Channel_code2)),
	     timer:sleep(?DELAY),
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE1),
             ok = server:publish(Consumer_code1, Channel_code2, ?MESSAGE2),
	     timer:sleep(?DELAY),
	     %% test get_messages single channel
	     ?assertEqual({ok,[?MESSAGE1]},
			  server:get_messages(Consumer_code2, Channel_code)),
	     ?assertEqual({ok,[]},
			  server:get_messages(Consumer_code2, Channel_code)),
	     %% test get rest of the channels after getting one channnel
	     {ok, Messages4} = server:get_messages(Consumer_code2),
	     ?assertEqual({ok,[?MESSAGE2]}, dict:find(Channel_code2, Messages4)),
	     ?assertEqual(error, dict:find(Channel_code, Messages4)),
             %% tests unsubscribe
             ?assertEqual(ok, server:unsubscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:unsubscribe(Consumer_code2, Channel_code)),
	     % make sure to wait until consumers unregister themselves
	     timer:sleep(?DELAY),
             ?assertEqual({ok,[Channel_code2, Channel_code]}, server:get_channels()),
             ?assertEqual({ok,[]}, server:get_subscribtions(Consumer_code1)),
             ?assertEqual({ok,[Channel_code2]}, server:get_subscribtions(Consumer_code2)),
             ?assertEqual(ok, server:unsubscribe(Consumer_code2, Channel_code2)),
             ?assertEqual({ok,[]}, server:get_subscribtions(Consumer_code2)),
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
