%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% adapter between rest and server apps
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(r_server_adapter).

%% API
-export([handle_request/1]).

%%%===================================================================
%%% API
%%%===================================================================
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec handle_request(cowboy_req:req()) -> {[{<<_:64>>, undefined | binary()},...]}.
handle_request(Req) ->
    {Channel, Req2} = cowboy_req:binding(channel, Req),
    {Existed_session_id, Req3} = r_utils:get_session(Req2),
    {Method, Req4} = cowboy_req:method(Req3),
    Message = [1,2,3],
    
    case Method of
	<<"GET">> ->
	    {ok, Session_id, Messages} = server:get_messages(Existed_session_id,
							     Channel);
	<<"POST">> ->
	    Messages = [post],
	    {ok, Session_id} = server:publish(Existed_session_id,
					      Channel, Message);
	
	<<"PUT">> ->
	    Messages = [put],
	    {ok, Session_id} = server:subscribe(Existed_session_id,Channel)
		

    end,
    
    Response = {[{<<"channel">>, channel},
		 {<<"session_id">>, Session_id},
		 {<<"messages">>, Messages}]},

    {Response, Req4}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
