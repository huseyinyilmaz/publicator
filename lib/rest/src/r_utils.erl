%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% utility functions for rest application
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(r_utils).

%% API
-export([get_session/1, set_session/2, drop_session/1]).

-define(COOKIE_NAME, <<"PUBLICATOR_SESSON_ID">>).
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
    %% XXX hulloo_session id must not be hardcoded here
    Req2 = cowboy_req:set_resp_header(<<"Set-Cookie">>,<<"PUBLICATOR_SESSION_ID=deleted; expires=Thu, 01-Jan-1970 00:00:01 GMT; path=/">>, Req),
    Req2.


%%%===================================================================
%%% Internal functions
%%%===================================================================
