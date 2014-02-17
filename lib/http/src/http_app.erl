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
		    [{"/session/[...]" , h_session_handler, []},
                     {"/:session/http/", h_http_handler,[]},
		     {"/:session/ws/", websocket_handler, []}
		    ]}
                 ]),

    Pool_count = s_utils:get_env(http, pool_count, 100),
    Http_port = s_utils:get_env(http, port, 8766),
    Ssl_port = s_utils:get_env(http, ssl_port, undefined),
    Cert_file = s_utils:get_env(http, certfile, undefined),
    Key_file =  s_utils:get_env(http, keyfile, undefined),
    
    %% start either ssl or ss
    if
        Ssl_port /= undefined, Cert_file /= undefined, Key_file /= undefined ->    
            {ok, _} = cowboy:start_https(https, Pool_count,
                                         [{port, Ssl_port},
                                          {certfile, Cert_file},
                                          {keyfile, Key_file}],
                                         [{env, [{dispatch, Dispatch}]}]);
        true ->
            {ok, _} = cowboy:start_http(http, Pool_count,
                                        [{port, Http_port}],
                                        [{env, [{dispatch, Dispatch}]}])
    end,
    
    http_sup:start_link().

-spec stop(_State::any()) -> ok.
stop(_State) ->
    ok.
