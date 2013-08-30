%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% utility functions for http application
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(h_utils).

%% API
-export([get_or_create_session/1, drop_session/1]).

-define(COOKIE_NAME, <<"publicator-session-id">>).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_session(cowboy_req:req()) -> {undefined | binary(),cowboy_req:req()}.
get_session(Req) ->
    cowboy_req:cookie(?COOKIE_NAME, Req).


-spec set_session(cowboy_req:req(),binary()|
		  maybe_improper_list(binary() |
				      maybe_improper_list(any(),binary() |
							  []) |
				      byte(),binary() |
				      [])) -> {binary() |
					       maybe_improper_list(
						 binary() |
						 maybe_improper_list(any(),binary() |
								     []) |
						 byte(),binary() |
						 []), cowboy_req:req()}.
set_session(Req, Session_id) ->
    Req1 = cowboy_req:set_resp_cookie(?COOKIE_NAME, Session_id, [{path, <<"/">>}], Req),
    {Session_id, Req1}.


-spec drop_session(cowboy_req:req()) -> cowboy_req:req().
drop_session(Req) ->
    Req2 = cowboy_req:set_resp_header(
	     <<"Set-Cookie">>,
	     <<?COOKIE_NAME/bitstring,
	       <<"=deleted; expires=Thu, 01-Jan-1970 00:00:01 GMT; path=/">>/bitstring>>, Req),
    Req2.


get_or_create_session(Req) ->
    {Existed_session_id, Req1} = get_session(Req),
    case Existed_session_id of
	undefined -> Session_id = s_utils:generate_code(),
		     {Session_id, Req2} = set_session(Req1, Session_id);
	_ -> Session_id = Existed_session_id,
	     Req2 = Req1
    end,
    {Session_id, Req2}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
