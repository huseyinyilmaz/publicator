%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

-spec start(normal | {takeover, node()} | {failover, node()},
            any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                      {error, Reason::any()}.
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile(
		 [
		  {s_utils:get_env(rest, host, '_'), % host match
		    [
		     {"/channels/", r_channel_handler, []},
		     {"/subscribtions/", r_subscribtion_get_handler, []},
		     {"/subscribtions/[:channel/]", r_subscribtion_handler, []},
		     {"/messages/", r_message_get_handler, []},
		     {"/messages/[:channel]", r_message_handler, []}
		    ]}
		  ]),
    {ok, _} = cowboy:start_http(http, s_utils:get_env(rest, pool_count, 100),
				[{port, s_utils:get_env(rest, port, 8766)}],
				[{env, [{dispatch, Dispatch}]}
	]),
    rest_sup:start_link().

-spec stop(_State::any()) -> ok.
stop(_State) ->
	ok.
