%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(p_websocket_handler).
-behaviour(cowboy_websocket_handler).
%% API
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-record(state,{session_id, producer_pid, producer_monitor_ref}).
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
    case publicator_core:get_producer(Session_id) of
	{ok, Producer_pid} ->
            ok = publicator_core:add_message_handler(Session_id, self()),
	    Producer_monitor_ref = monitor(process, Producer_pid),
            State = #state{session_id=Session_id,
        		   producer_pid=Producer_pid,
			   producer_monitor_ref=Producer_monitor_ref},
            lager:debug("Initializing websocket handler ~p", [Session_id]);
	{error, not_found}->
	    State = #state{session_id=no_session, producer_pid=undefined,
			   producer_monitor_ref=undefined},
	    lager:info("Given session id ~p does not exist",[Session_id])
    end,
    {ok, Req1, State}.


websocket_handle({text, _Raw_data}, Req, #state{session_id=no_session}=State) ->
    Result = p_utils:no_session_response(),
    {reply, {text, Result}, Req, State};

websocket_handle({text, Text}, Req, #state{session_id=Session_id}=State) ->
    lager:debug("======================== Websocket Debug Start ============", []),
    lager:debug("Text=~p~n",[Text]),
    {Session_id, Req_session} = cowboy_req:binding(session, Req), % assert Session_id
    Msg = p_utils:parse_request_text(Text, Session_id),
    Body = p_generic_handler:handle_request(Msg),
    {reply, {text, Body}, Req_session, State}.

websocket_info({'DOWN', Ref, process, Producer_pid, Reason},
               Req, #state{session_id=Session_id,
                           producer_pid=Producer_pid,
                           producer_monitor_ref=Ref}=State)->
    
    lager:warning("Producer process ~p died for reason ~p", [Session_id, Reason]),
    Result = p_utils:make_response(<<"error">>, <<"Producer_handler is died unexpectedly">>,
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
    Result = p_utils:message_response(Message),
    {reply, {text, Result}, Req, State}.

handle_terminate(#state{session_id=no_session})-> ok;
handle_terminate(#state{session_id=Session_id})->
    lager:debug("Terminate websocket handler for session ~p", [Session_id]),
    %% XXX do not stop producer?
    ok = publicator_core:stop_producer(Session_id),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
