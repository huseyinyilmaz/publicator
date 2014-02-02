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
            log(State, "Initializing websocket handler State=~p", [State]);
	{error, not_found}->
	    State = #state{session_id=no_session, consumer_pid=undefined,
			   consumer_monitor_ref=undefined},
	    log(State, "Given session id does not exist(~p)",[Session_id])
		
    end,
    {ok, Req1, State}.

websocket_handle({text, Raw_data}, Req, State) ->
    log(State, "Got request raw request=~p~n", [Raw_data]),
    Request_data = jiffy:decode(Raw_data),
    log(State, "Processed request=~p~n", [Request_data]),
    handle_request(Request_data, Req, State).

websocket_info({'DOWN', Ref, process, Consumer_pid, Reason},
     Req, #state{consumer_pid=Consumer_pid,
		 consumer_monitor_ref=Ref}=State)->
    
    log(State, "Consumer process died reason = ~p", [Reason]),
    Result = h_utils:make_response(<<"error">>, <<"Consumer_handler is died unexpectedly">>,
			   [{<<"reason_for_dead_process">>, Reason}]),
    {reply, {text, Result}, Req, State};
    

websocket_info(Data, Req, State) ->
    log(State, "Got an info request=~p~n",[Data]),
    handle_info(Data, Req, State).

websocket_terminate(_Reason, _Req, State) ->
    handle_terminate(State).


%%%===================================================================
%%% WEBSOCKET HANDLERS
%%%===================================================================

%% Handle incoming requests
handle_request(_Request_data, Req, #state{session_id=no_session}=State)->
    Result = h_utils:no_session_response(),
    {reply, {text, Result}, Req, State};
handle_request(Request_data, Req, State)->
    
    %% Make sure that message format is valid
    case Request_data of
	{[{<<"type">>, Type},
	  {<<"data">>, Data}]} -> 
	    handle_request(Type, Data, Req, State);
	{[{<<"data">>, Data},
	  {<<"type">>, Type}]} -> 
	    handle_request(Type, Data, Req, State);	
	_ ->
	    Result = h_utils:make_response(<<"invalid_msg_format">>,
				   <<"Data format must be as {'type': 'typ', 'data': 'dt'}">>,
				   [{<<"invalid_data">>, Request_data}]),
	    {reply, {text, Result}, Req, State}
    end.


handle_request(<<"subscribe">>,
	       {[{<<"channel_code">>, Channel_code},
		 {<<"type">>, Handler_type_bin}]},
	       Req, State) ->
    handle_subscribe_request(Handler_type_bin, Channel_code, Req,State);

handle_request(<<"subscribe">>,
	       {[{<<"type">>, Handler_type_bin},
		 {<<"channel_code">>, Channel_code}]},
	       Req, State) ->
    handle_subscribe_request(Handler_type_bin, Channel_code, Req,State);

handle_request(<<"unsubscribe">>, Data, Req, #state{session_id=Session_id}=State) ->
    ok = server:unsubscribe(Session_id, Data),
    ok = server:remove_message_handler(Session_id, self()),
    Result = h_utils:make_response(<<"unsubscribed">>,
			   Data),
    {reply, {text, Result}, Req, State};

handle_request(<<"get_subscribtions">>, _Data, Req, #state{session_id=Session_id}=State) ->
    {ok, Subscribtion_data} = server:get_subscribtions(Session_id),
    Result = h_utils:make_response(<<"subscribtions">>, Subscribtion_data),
    {reply, {text, Result}, Req, State};

handle_request(<<"publish">>,
	       {[{<<"channel_code">>,Channel_code},
		 {<<"message">>,Message}]},
	       Req, State) ->
    handle_publish_request(Channel_code, Message, Req,State);

handle_request(<<"publish">>,
	       {[{<<"message">>,Message},
		 {<<"channel_code">>,Channel_code}]},
	       Req, State) ->
    handle_publish_request(Channel_code, Message, Req,State);

handle_request(<<"get_consumers">>,
	       {[{<<"channel_code">>,Channel_code}]},
	       Req, #state{session_id=Session_id}=State) ->
    {Headers, Req2} = cowboy_req:headers(Req),
    {ok, Consumers_data} = server:get_consumers(Session_id,
                                                Channel_code,
                                                Headers),
    Result = h_utils:make_response(<<"consumers">>,Consumers_data,
                                  [{<<"channel_code">>, Channel_code}]),
    {reply, {text, Result}, Req2, State};
    
handle_request(Type, Data, Req, #state{session_id=Session_id}=State)->
    Result = h_utils:make_response(<<"unhandled_msg">>, Data,
			   [{<<"request_type">>, Type},
			    {<<"session">>, Session_id}]),
    {reply, {text, Result}, Req, State}.

handle_info({message, Channel_code, Message}, Req, State)->
    Result = h_utils:make_response(<<"message">>, Message,
			  [{<<"channel_code">>, Channel_code}]),
    {reply, {text, Result}, Req, State};

handle_info({cached_message, Channel_code, Message}, Req, State)->
    Result = h_utils:make_response(<<"cached_message">>, Message,
                                   [{<<"channel_code">>, Channel_code}]),
    {reply, {text, Result}, Req, State};

handle_info({add_subscribtion, Channel_code, Consumer_code}, Req, State)->
    Result = h_utils:make_response(<<"add_subscribtion">>, Consumer_code,
			  [{<<"channel_code">>, Channel_code}]),
    {reply, {text, Result}, Req, State};

handle_info({remove_subscribtion, Channel_code, Consumer_code}, Req, State)->
    Result = h_utils:make_response(<<"remove_subscribtion">>, Consumer_code,
			  [{<<"channel_code">>, Channel_code}]),
    {reply, {text, Result}, Req, State};

handle_info(Msg,Req,State)->
    Result = h_utils:make_response(<<"unhandled_info">>, tuple_to_list(Msg)),
    {reply, {text, Result}, Req, State}.

handle_terminate(#state{session_id=no_session})-> ok;
handle_terminate(#state{session_id=Session_id}=State)->
    log(State, "Terminate websocket handler"),
    ok = server:stop_consumer(Session_id),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
handle_publish_request(Channel_code, Message, Req,
                       #state{session_id=Session_id}=State) ->
    {Headers, Req2} = cowboy_req:headers(Req),
    case server:publish(Session_id, Channel_code, Message, Headers) of
        ok -> {ok, Req2, State};
        {error, consumer_not_found} ->
            {reply,
             {text, h_utils:make_response(<<"error">>, <<"consumer_not_found">>)},
             Req2,
             State};
        {error, permission_denied} ->
            {reply,
             {text, h_utils:make_response(<<"error">>, <<"permission_denied">>)},
             Req2,
             State}
    end.

handle_subscribe_request(Handler_type_bin,
			 Channel_code, Req,
			 #state{session_id=Session_id}=State) ->
    Handler_type = case Handler_type_bin of
		       <<"message_only">> -> message_only;
		       <<"all">> -> all
		   end,
    {Headers, Req2} = cowboy_req:headers(Req),
    case server:subscribe(Session_id, Channel_code, Handler_type, Headers) of
	{error, invalid_channel_code} ->
	    Result = h_utils:make_response(<<"error">>, <<"invalid_channel_code">>);
        {error, consumer_not_found} ->
            Result = h_utils:make_response(<<"error">>, <<"consumer_not_found">>);
        {error, permission_denied} ->
            Result = h_utils:make_response(<<"error">>, <<"permission_denied">>);
        
	ok->
	    ok = server:add_message_handler(Session_id, self()),
	    Result = h_utils:make_response(<<"subscribed">>, Channel_code)
	    
    end,
    {reply, {text, Result}, Req2, State}.


log(State, String)->
    log(State,String,[]).

log(#state{session_id=Session_id}=_State, String, Args) ->
    Log_msg = lists:concat(["~p - ", String]),
    %% M = io_lib:format(Log_msg,[Session_id|Args]),
    %% lager:info(M).
    lager:info(Log_msg, [Session_id|Args]).
