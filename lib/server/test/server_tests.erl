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
    lager:info("Setup server"),
    ok = application:start(sasl),
    ok = application:start(gproc),
    ok = server:start().

cleanup_server(_)->
    lager:info("Cleanup server"),
    ok = server:stop(),
    ok = application:stop(gproc),
    ok = application:stop(sasl).


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
			  server:subscribe(Consumer_code, Channel_code, message_only)),
	     ?assertEqual({error, consumer_not_found}, server:unsubscribe(Consumer_code, Channel_code)),
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
             ?assertEqual(ok, server:subscribe(Consumer_code1, Channel_code, message_only)),
             ?assertEqual(ok, server:subscribe(Consumer_code1, Channel_code2, message_only)),
             ?assertEqual(ok, server:subscribe(Consumer_code2, Channel_code, message_only)),
	     {ok, Consumer_list1} = server:get_consumers(Channel_code),
             ?assertEqual(lists:sort([Consumer_code1, Consumer_code2]),
			  lists:sort(Consumer_list1)),
             ?assertEqual({ok, [Consumer_code1]},
			  server:get_consumers(Channel_code2)),
	     
	     %% test get channels
	     {ok, Channel_list} = server:get_channels(),
             ?assertEqual(lists:sort([Channel_code, Channel_code2]),
			  lists:sort(Channel_list)),
	     %% test get subscribtions
             ?assertEqual({ok,[Channel_code2, Channel_code]},
			  server:get_subscribtions(Consumer_code1)),
	     ?assertEqual({ok,[Channel_code]},
			  server:get_subscribtions(Consumer_code2)),
             %% tests unsubscribe
             ?assertEqual(ok, server:unsubscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:unsubscribe(Consumer_code2, Channel_code)),
	     {ok, Channel_list2} = server:get_channels(),
             ?assertEqual(lists:sort([Channel_code, Channel_code2]),
			  lists:sort(Channel_list2)),
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
             ?assertEqual(ok, server:subscribe(Consumer_code1, Channel_code, message_only)),
             ?assertEqual(ok, server:subscribe(Consumer_code2, Channel_code, message_only)),
	     %% test send message
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE1),
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE2),
	     timer:sleep(?DELAY),
	     %% test get_messages
	     {ok, Messages} = server:get_messages(Consumer_code2),
	     ?assertEqual({ok,[?MESSAGE1, ?MESSAGE2]}, dict:find(Channel_code, Messages)),
	     {ok, Messages2} = server:get_messages(Consumer_code1),
	     ?assertEqual({ok,[?MESSAGE1, ?MESSAGE2]}, dict:find(Channel_code, Messages2)),
	     
	     %% make sure that Messages has been cleared
	     {ok, Messages3} = server:get_messages(Consumer_code2),
	     ?assertEqual(error, dict:find(Channel_code, Messages3)),
	     %% make usre that original sender did not get the messages
	     {ok, Messages4} = server:get_messages(Consumer_code1),
	     ?assertEqual(error, dict:find(Channel_code, Messages4)),
	     %% make sure that channels are seperate
             ?assertEqual(ok, server:subscribe(Consumer_code2, Channel_code2, message_only)),
             ok = server:publish(Consumer_code1, Channel_code, ?MESSAGE1),
             ok = server:publish(Consumer_code1, Channel_code2, ?MESSAGE2),
	     timer:sleep(?DELAY),
	     %% test get_messages single channel
	     ?assertEqual({ok,[?MESSAGE1]},
			  server:get_messages(Consumer_code2, Channel_code)),
	     ?assertEqual({ok,[]},
			  server:get_messages(Consumer_code2, Channel_code)),
	     %% test get rest of the channels after getting one channnel
	     {ok, Messages5} = server:get_messages(Consumer_code2),
	     ?assertEqual({ok,[?MESSAGE2]}, dict:find(Channel_code2, Messages5)),
	     ?assertEqual(error, dict:find(Channel_code, Messages5)),
             %% tests unsubscribe
             ?assertEqual(ok, server:unsubscribe(Consumer_code1, Channel_code)),
             ?assertEqual(ok, server:unsubscribe(Consumer_code2, Channel_code))
	 end)
     }}.


server_handler_mode_test_() ->
    {setup,
     fun setup_server/0,
     fun cleanup_server/1,
     {"",
      ?_test(begin
		 %% Create consumers
		 {ok, Consumer_code1, Consumer_pid1} = server:create_consumer(),
		 {ok, Consumer_code2, Consumer_pid2} = server:create_consumer(),
		 Channel_code = ?CHANNEL1,
		 Channel_code2 = ?CHANNEL2,
		 Message1 = <<"Message1">>,
		 Message2 = <<"Message2">>,
		 timer:sleep(?DELAY),
		 %% Add consumers to channels

		 ?assertEqual(ok,
			      server:subscribe(Consumer_code1, Channel_code, message_only)),
		 ?assertEqual(ok,
			      server:subscribe(Consumer_code2, Channel_code, message_only)),
		 ?assertEqual(ok,
			      server:subscribe(Consumer_code1, Channel_code2, message_only)),
		 ?assertEqual(ok,
			      server:subscribe(Consumer_code2, Channel_code2, message_only)),

		 Mock1_pid = process_mock:make_message_receiver(self(), mock1),
		 Mock2_pid = process_mock:make_message_receiver(self(), mock2),
		 Mock3_pid = process_mock:make_message_receiver(self(), mock3),
		 Mock4_pid = process_mock:make_message_receiver(self(), mock4),

		 server:add_message_handler(Consumer_code1, Mock1_pid),
		 server:add_message_handler(Consumer_code2, Mock2_pid),
		 server:add_message_handler(Consumer_code2, Mock3_pid),
		 server:add_message_handler(Consumer_code2, Mock4_pid),

		 Expected_msg1 = {message,
				  Channel_code,
				  Message1},
		 Expected_msg2 = {message,
				  Channel_code2,
				  Message2},

		 s_consumer:publish(Consumer_pid1, Channel_code, Message1),
		 s_consumer:publish(Consumer_pid2, Channel_code2, Message2),
		 timer:sleep(?DELAY),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock2)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock3)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock4)),
		 ?assertEqual(Expected_msg1, process_mock:receive_message(mock1)),
		 ?assertEqual(Expected_msg2, process_mock:receive_message(mock1))
	     end)}}.
