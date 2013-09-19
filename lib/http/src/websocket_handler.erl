%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(websocket_handler).

%% API
-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-record(state,{session_id}).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec Initialize a new websocket connection
%% @end
%%--------------------------------------------------------------------

init(_Transport, Req, _Opts, _Active) ->
    error_logger:info_report("Initializing bullet handler"),
    {ok, Req, #state{session_id=undefined}}.

stream(Raw_data, Req, State) ->
    error_logger:info_report({raw_request, Raw_data}),
    Data = jiffy:decode(Raw_data),
    error_logger:info_report({processed_request, Data}),
    h_server_adapter:handle_request(Data, Req, State).

info(Data, Req, State) ->
    error_logger:info_report({info, Data}),
    h_server_adapter:handle_info(Data, Req, State).

terminate(_Req, State) ->
    error_logger:info_report(terminate_bullet_handler),
    h_server_adapter:terminate(State).


%%%===================================================================
%%% Internal functions
%%%===================================================================







