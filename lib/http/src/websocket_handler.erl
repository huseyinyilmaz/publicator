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
-record(state,{session_id, consumer_pid, consumer_monitor_ref}).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec Initialize a new websocket connection
%% @end
%%--------------------------------------------------------------------

init(_Transport, Req, _Opts, _Active) ->
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    case h_server_adapter:get_consumer(Session_id) of
	{ok, Consumer_pid} ->
            ok = h_server_adapter:add_message_handler(Session_id, self()),
	    Consumer_monitor_ref = monitor(process, Consumer_pid),
            State = #state{session_id=Session_id,
        		   consumer_pid=Consumer_pid,
			   consumer_monitor_ref=Consumer_monitor_ref},
            log(State, "Initializing bullet handler State=~p", [State]);
	{error, not_found}->
	    State = #state{session_id=no_session, consumer_pid=undefined,
			   consumer_monitor_ref=undefined},
	    log(State, "Given session id does not exist(~p)",[Session_id])
		
    end,
    {ok, Req1, State}.

stream(Raw_data, Req, State) ->
    log(State, "Got request raw request=~p~n", [Raw_data]),
    Request_data = jiffy:decode(Raw_data),
    log(State, "Processed request=~p~n", [Request_data]),
    handle_request(Request_data, Req, State).

info({'DOWN', Ref, process, Consumer_pid, Reason},
     Req, #state{consumer_pid=Consumer_pid,
		 consumer_monitor_ref=Ref}=State)->
    
    log(State, "Consumer process died reason = ~p", [Reason]),
    Result = h_utils:make_response(<<"error">>, <<"Consumer_handler is died unexpectedly">>,
			   [{<<"reason_for_dead_process">>, Reason}]),
    {reply, Result, Req, State};
    

info(Data, Req, State) ->
    log(State, "Got an info request=~p~n",[Data]),
    handle_info(Data, Req, State).

terminate(_Req, State) ->
    log(State, "Terminate bullet handler."),
    handle_terminate(State).


%%%===================================================================
%%% WEBSOCKET HANDLERS
%%%===================================================================

%% Handle incoming requests
handle_request(_Request_data, Req, #state{session_id=no_session}=State)->
    Result = h_utils:no_session_response(),
    {reply, Result, Req, State};
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
	    {reply, Result, Req, State}
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


handle_request(<<"full_subcribe">>, Data, Req, State) ->
    handle_subscribe_request(all, Data, Req,State);

handle_request(<<"unsubscribe">>, Data, Req, #state{session_id=Session_id}=State) ->
    ok = h_server_adapter:unsubscribe(Session_id, Data),
    ok = h_server_adapter:remove_message_handler(Session_id, self()),
    Result = h_utils:make_response(<<"unsubscribed">>,
			   Data),
    {reply, Result, Req, State};

handle_request(<<"get_subscribtions">>, _Data, Req, #state{session_id=Session_id}=State) ->
    {ok, Subscribtion_data} = h_server_adapter:get_subscribtions(Session_id),
    Result = h_utils:make_response(<<"subscribtions">>, Subscribtion_data),
    {reply, Result, Req, State};

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
	       Req, State) ->
    {ok, Consumers_data} = h_server_adapter:get_consumers(Channel_code),
    Result = h_utils:make_response(<<"consumers">>,Consumers_data),
    {reply, Result, Req, State};
    


handle_request(Type, Data, Req, #state{session_id=Session_id}=State)->
    Result = h_utils:make_response(<<"unhandled_msg">>, Data,
			   [{<<"request_type">>, Type},
			    {<<"session">>, Session_id}]),
    {reply, Result, Req, State}.

handle_info({message, Channel_code, Message}, Req, State)->
    Result = h_utils:make_response(<<"message">>, Message,
			  [{<<"channel_code">>, Channel_code}]),
    {reply, Result, Req, State};

handle_info({add_subscribtion, Channel_code, Consumer_code}, Req, State)->
    Result = h_utils:make_response(<<"add_subscribtion">>, Consumer_code,
			  [{<<"channel_code">>, Channel_code}]),
    {reply, Result, Req, State};

handle_info({remove_subscribtion, Channel_code, Consumer_code}, Req, State)->
    Result = h_utils:make_response(<<"remove_subscribtion">>, Consumer_code,
			  [{<<"channel_code">>, Channel_code}]),
    {reply, Result, Req, State};

handle_info(Msg,Req,State)->
    Result = h_utils:make_response(<<"unhandled_info">>, tuple_to_list(Msg)),
    {reply, Result, Req, State}.

handle_terminate(#state{session_id=no_session})-> ok;
handle_terminate(#state{session_id=Session_id}=State)->
    log(State, "Terminate websocket handler"),
    ok = h_server_adapter:remove_message_handler(Session_id, self()),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
handle_publish_request(Channel_code, Message, Req,
		    #state{session_id=Session_id}=State) ->
    ok = h_server_adapter:publish(Session_id, Channel_code, Message),
    {ok, Req, State}.

handle_subscribe_request(Handler_type_bin, Channel_code, Req, #state{session_id=Session_id}=State) ->
    Handler_type = case Handler_type_bin of
		       <<"message_only">> -> message_only;
		       <<"all">> -> all
		   end,
    case h_server_adapter:subscribe(Session_id, Channel_code, Handler_type) of
	{error, invalid_channel_code} ->
	    Result = h_utils:make_response(<<"error">>, <<"invalid_channel_code">>);
	ok->
	    ok = h_server_adapter:add_message_handler(Session_id, self()),
	    Result = h_utils:make_response(<<"subscribed">>, Channel_code)
	    
    end,
    {reply, Result, Req, State}.


log(State, String)->
    log(State,String,[]).

log(#state{session_id=Session_id}=_State, String, Args) ->
    Log_msg = lists:concat(["~p - ", String]),
    M = io_lib:format(Log_msg,[Session_id|Args]),
    lager:info(M).
