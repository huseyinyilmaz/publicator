%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(http_app).
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
		  {s_utils:get_env(http, host, '_'), % host match
		    [
		     %% {"/:session/subscribtions/", h_subscribtion_get_handler, []},
		     %% {"/:session/subscribtions/[:channel/]", h_subscribtion_handler, []},
		     %% {"/:session/messages/", h_message_get_handler, []},
		     %% {"/:session/messages/[:channel]", h_message_handler, []},
		     {"/session/[...]" , h_session_get_handler, []},
                     {"/:session/http/", h_http_handler,[]},
		     {"/:session/ws/", websocket_handler, []},
		     {"/", cowboy_static, {file, <<"./www/index.html">>}},
		     {"/[...]", cowboy_static, {dir, <<"./www">>}}
		    ]}
                 ]),
    case s_utils:get_env(http, connection, http) of
        http ->
            {ok, _} = cowboy:start_http(http, s_utils:get_env(http, pool_count, 100),
                                         [{port, s_utils:get_env(http, port, 8766)}],
                                         [{env, [{dispatch, Dispatch}]}]) ;
        ssl ->
            Cert_file = s_utils:get_env(http, certfile, []),
            Key_file =  s_utils:get_env(http, keyfile, []),
            {ok, _} = cowboy:start_https(https, s_utils:get_env(http, pool_count, 100),
                                         [{port, s_utils:get_env(http, port, 8766)},
                                          {certfile, Cert_file},
                                          {keyfile, Key_file}],
                                         [{env, [{dispatch, Dispatch}]}
                                         ])
    end,
    http_sup:start_link().

-spec stop(_State::any()) -> ok.
stop(_State) ->
    ok.
