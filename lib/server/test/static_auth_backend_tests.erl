-module(static_auth_backend_tests).
-include_lib("eunit/include/eunit.hrl").
-export([setup_server/0, cleanup_server/1]).
-export([setup_server_open_all_permissions/0]).
-export([setup_server_close_all_permissions/0]).

-define(CHANNEL1, <<"channelcode1">>).
-define(CHANNEL2, <<"channelcode2">>).
-define(MESSAGE1, <<"message1">>).
-define(MESSAGE2, <<"message2">>).
-define(AUTH_INFO, <<"test_auth_code">>).
-define(EXTRA_DATA, []).

-define(DELAY, 100).

setup_server() ->
    lager:debug("Setup server"),
    ok = application:start(sasl),
    ok = server:start().

setup_server_open_all_permissions() ->
    Configuration = {publicator_static_auth_backend,
                     [{consumer_code, all},
                      {group, all},
                      {auth_info, all}]},
    s_utils:set_env(server, auth_backend, Configuration),
    setup_server().

setup_server_close_all_permissions() ->
    Configuration = {publicator_static_auth_backend,
                     [{consumer_code, <<"closed">>},
                      {group, group1},
                      {auth_info, <<"closed">>}]},
    s_utils:set_env(server, auth_backend, Configuration),
    setup_server().

cleanup_server(_) ->
    lager:debug("Cleanup server"),
    ok = server:stop(),
    ok = application:stop(sasl).

server_opened_auth_test_() ->
    {setup,
     fun setup_server_open_all_permissions/0,
     fun cleanup_server/1,
     {"Test all permissions enabled.",
      ?_test(
         begin
	     {ok, Consumer_code1, _} = server:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
             ?assertEqual(ok, server:subscribe(Consumer_code1, ?CHANNEL1, message_only)),
             ok = server:publish(Consumer_code1, ?CHANNEL1, ?MESSAGE1),
             timer:sleep(?DELAY),
	     {ok, Messages} = server:get_messages(Consumer_code1),
	     ?assertEqual({ok,[{message, ?MESSAGE1}]}, dict:find(?CHANNEL1, Messages))
         end)
     }}.

server_closed_auth_test_() ->
    {setup,
     fun setup_server_close_all_permissions/0,
     fun cleanup_server/1,
     {"Test all permissions disabled.",
      ?_test(
         begin
	     {ok, Consumer_code1, _} = server:create_consumer(?AUTH_INFO, ?EXTRA_DATA),
             ?assertEqual(ok, server:subscribe(Consumer_code1, ?CHANNEL1, message_only)),
             ok = server:publish(Consumer_code1, ?CHANNEL1, ?MESSAGE1),
             timer:sleep(?DELAY),
	     {ok, Messages} = server:get_messages(Consumer_code1),
	     ?assertEqual({ok,[{message, ?MESSAGE1}]}, dict:find(?CHANNEL1, Messages))
         end)
     }}.
