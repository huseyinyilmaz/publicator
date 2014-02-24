%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(websocket_handler).
-behaviour(cowboy_websocket_handler).
%% API
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-record(state,{session_id, consumer_pid, consumer_monitor_ref}).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initialize a new websocket connection
%% @end
%%--------------------------------------------------------------------
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket};

init({ssl, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    case server:get_consumer(Session_id) of
	{ok, Consumer_pid} ->
            ok = server:add_message_handler(Session_id, self()),
	    Consumer_monitor_ref = monitor(process, Consumer_pid),
            State = #state{session_id=Session_id,
        		   consumer_pid=Consumer_pid,
			   consumer_monitor_ref=Consumer_monitor_ref},
            lager:debug("Initializing websocket handler ~p", [Session_id]);
	{error, not_found}->
	    State = #state{session_id=no_session, consumer_pid=undefined,
			   consumer_monitor_ref=undefined},
	    lager:info("Given session id ~p does not exist",[Session_id])
    end,
    {ok, Req1, State}.


websocket_handle({text, _Raw_data}, Req, #state{session_id=no_session}=State) ->
    Result = h_utils:no_session_response(),
    {reply, {text, Result}, Req, State};

websocket_handle({text, Raw_data}, Req, #state{session_id=Session_id}=State) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req), % assert Session_id
    Request_data = jiffy:decode(Raw_data),
    {Request_plist} = Request_data,
    Request_type = proplists:get_value(<<"type">>, Request_plist),
    {Headers, Req2} = cowboy_req:headers(Req1),
    Body = h_generic_handler:handle_request(Request_type, Session_id, Request_data, Headers),
    {reply, {text, Body}, Req2, State}.

websocket_info({'DOWN', Ref, process, Consumer_pid, Reason},
               Req, #state{session_id=Session_id,
                           consumer_pid=Consumer_pid,
                           consumer_monitor_ref=Ref}=State)->
    
    lager:warning("Consumer process ~p died for reason ~p", [Session_id, Reason]),
    Result = h_utils:make_response(<<"error">>, <<"Consumer_handler is died unexpectedly">>,
			   [{<<"reason_for_dead_process">>, Reason}]),
    {reply, {text, Result}, Req, State};
    

websocket_info(Data, Req, State) ->
    handle_info(Data, Req, State).

websocket_terminate(_Reason, _Req, State) ->
    handle_terminate(State).


%%%===================================================================
%%% WEBSOCKET HANDLERS
%%%===================================================================

handle_info(Message, Req,State)->
    Result = h_utils:message_response(Message),
    {reply, {text, Result}, Req, State}.

handle_terminate(#state{session_id=no_session})-> ok;
handle_terminate(#state{session_id=Session_id})->
    lager:debug("Terminate websocket handler for session ~p", [Session_id]),
    %% XXX do not stop consumer?
    ok = server:stop_consumer(Session_id),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
