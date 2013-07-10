%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(r_toppage_handler).

-export([init/3]).
-export([content_types_provided/2, allowed_methods/2, known_methods/2
	 ,content_types_accepted/2
	]).
-export([handle_json/2, from_text/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
        error_logger:info_report(content_types_provided),
    {[
      %% {{<<"application">>, <<"json">>, []}, handle_json}
      {{<<"*">>, <<"*">>,[]}, handle_json}
     ], Req, State}.

content_types_accepted(Req, State)->
    error_logger:info_report(content_types_accepted),
    {[
      %% {{<<"application">>,<<"json">>, []}, from_text}
      {'*', from_text}
     ], Req, State}.

known_methods(Req, State) ->
    {['GET', 'POST', 'PUT', 'DELETE'], Req, State}.

allowed_methods(Req, State) ->
    {['GET', 'POST', 'PUT', 'DELETE'], Req, State}.

handle_json(Req, State) ->
    error_logger:info_report(handle_json),
    {Response, Req1} = r_server_adapter:handle_request(Req),
    Body = jiffy:encode(Response),
    {Body, Req1, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
from_text(Req, State) ->
	{true, Req, State}.
