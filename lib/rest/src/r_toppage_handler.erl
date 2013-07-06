%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(r_toppage_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([handle_json/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, handle_json},
		{<<"application/json">>, handle_json},
		{<<"text/plain">>, handle_json}
	], Req, State}.


handle_json(Req, State) ->
    {Response, Req1} = r_server_adapter:handle_request(Req),
    Body = jiffy:encode(Response),
    {Body, Req1, State}.
