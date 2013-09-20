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
    {Session_id, Req1} = cowboy_req:binding(session, Req),
    io:format("XXX Req=~p~n Opts=~p~n",[Req, _Opts]),
    {ok, Req1, #state{session_id=Session_id}}.

stream(Raw_data, Req, State) ->
    error_logger:info_report({raw_request, Raw_data}),
    Request_data = jiffy:decode(Raw_data),
    error_logger:info_report({processed_request, Request_data}),
    handle_request(Request_data, Req, State).

info(Data, Req, State) ->
    error_logger:info_report({info, Data}),
    handle_info(Data, Req, State).

terminate(_Req, State) ->
    error_logger:info_report(terminate_bullet_handler),
    handle_terminate(State).


%%%===================================================================
%%% WEBSOCKET HANDLERS
%%%===================================================================

%% Handle incoming requests
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
	    Result = make_response(<<"invalid_msg_format">>,
				   <<"Data format must be as {'type': 'typ', 'data': 'dt'}">>,
				   [{<<"invalid_data">>, Request_data}]),
	    {reply, Result, Req, State}
    end.





handle_request(<<"subscribe">>, Data, Req, #state{session_id=Session_id}=State) ->
    ok = h_server_adapter:subscribe(Session_id, Data, self()),
    Result = make_response(<<"subscribed">>,
			   Data),
    {reply, Result, Req, State};

handle_request(<<"unsubscribe">>, Data, Req, #state{session_id=Session_id}=State) ->
    ok = h_server_adapter:unsubscribe(Session_id, Data, self()),
    Result = make_response(<<"unsubscribed">>,
			   Data),
    {reply, Result, Req, State};

handle_request(<<"get_subscribtions">>, _Data, Req, #state{session_id=Session_id}=State) ->
    {ok, Subscribtion_data} = h_server_adapter:get_subscribtions(Session_id),
    Result = make_response(<<"subscribtions">>, Subscribtion_data),
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

	
handle_request(Type, Data, Req, #state{session_id=Session_id}=State)->
    Result = make_response(<<"unhandled_msg">>, Data,
			   [{<<"request_type">>, Type},
			    {<<"session">>, Session_id}]),
    {reply, Result, Req, State}.

handle_info({message, Channel_code, Message}, Req, State)->
    Result = make_response(<<"message">>, Message,
			  [{<<"channel_code">>, Channel_code}]),
    {reply, Result, Req, State};
    
handle_info(Msg,Req,State)->
    Result = make_response(<<"unhandled_info">>, tuple_to_list(Msg)),
    {reply, Result, Req, State}.

handle_terminate(_State)-> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_response(Type, Data) ->
    make_response(Type,Data,[]).

make_response(Type, Data, Extra_list) ->
    jiffy:encode({[{<<"type">>, Type},
		   {<<"data">>, Data}| Extra_list]}).
    
handle_publish_request(Channel_code, Message, Req,
		    #state{session_id=Session_id}=State) ->
    ok = h_server_adapter:publish(Session_id, Channel_code, Message),
    {ok, Req, State}.
    
