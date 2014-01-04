%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(h_toppage_handler).

-export([init/3]).
-export([content_types_provided/2, allowed_methods/2, known_methods/2
	 ,content_types_accepted/2
	]).
-export([handle_json/2, process_json/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

known_methods(Req, State) ->
    {
      [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>]
     %% ['GET', 'POST', 'PUT', 'DELETE']

      , Req, State}.

allowed_methods(Req, State) ->
    {
      [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>]
     %% ['GET', 'POST', 'PUT', 'DELETE']
      , Req, State}.


%% processes given request.
content_types_accepted(Req, State)->
    lager:info(content_types_accepted),
    {[
      {{<<"application">>, <<"json">>, []}, process_json},
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, process_json}],
     Req, State}.


%% creates response. gets Req and State from c_t_accepted method
content_types_provided(Req, State) ->
    lager:info(content_types_provided),
	{[
	  {{<<"text">>, <<"plain">>, []}, handle_json},
	  {{<<"text">>, <<"html">>, []}, handle_json},
	  {{<<"application">>, <<"json">>, []}, handle_json}
	], Req, State}.

process_json(Req, State) ->
    lager:info(process_json),
    {Response, Req1} = r_server_adapter:handle_request(Req),
    Body = jiffy:encode(Response),
    {Body, Req1, State}.
    

handle_json(Req, State) ->
    lager:info(handle_json),
    %% {Response, Req1} = r_server_adapter:handle_request(Req),
    Body = jiffy:encode([1234]),
    {Body, Req, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
