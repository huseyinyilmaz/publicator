%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(publicator_app).
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
		  {p_utils:get_env(host, '_'), % host match
		    [{"/session/[...]" , p_session_handler, []},
                     {"/:session/http/", p_http_handler,[]},
		     {"/:session/ws/", p_websocket_handler, []}
		    ]}
                 ]),

    Pool_count = p_utils:get_env(pool_count, 100),
    Http_port = p_utils:get_env(port, 8766),
    Ssl_port = p_utils:get_env(ssl_port, undefined),
    Cert_file = p_utils:get_env(certfile, undefined),
    Key_file =  p_utils:get_env(keyfile, undefined),
    
    %% start either ssl or ss
    {ok, _} = if
        Ssl_port /= undefined, Cert_file /= undefined, Key_file /= undefined ->    
            cowboy:start_https(https, Pool_count,
                               [{port, Ssl_port},
                                {certfile, Cert_file},
                                {keyfile, Key_file}],
                               [{env, [{dispatch, Dispatch}]}]);
        true ->
            cowboy:start_http(http, Pool_count,
                              [{port, Http_port}],
                              [{env, [{dispatch, Dispatch}]}])
    end,
    
    publicator_sup:start_link().

-spec stop(_State::any()) -> ok.
stop(_State) ->
    ok.
