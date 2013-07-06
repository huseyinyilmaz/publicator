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
    {Category_name, Req2} = cowboy_req:binding(category, Req),
    {Resource_name, Req3} = cowboy_req:binding(resource, Req2),
    {Existed_session_id, Req4} = r_utils:get_session(Req3),
    {Method, Req5} = cowboy_req:method(Req4),
    Message = [1,2,3],
    
    case Method of
	<<"GET">> ->
	    {ok, Session_id, Messages} = server:get_messages(Existed_session_id,
							     Category_name, Resource_name);
	<<"POST">> ->
	    Messages = [post],
	    {ok, Session_id} = server:publish(Existed_session_id,
					      Category_name, Resource_name,
					      Message)

    end,
    Response = {[{<<"category">>, Category_name},
		 {<<"resource">>, Resource_name},
		 {<<"session_id">>, Session_id},
		 {<<"messages">>, Messages}]},

    {Response, Req5}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
