%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(s_static_auth_backend).
-behivour(s_auth_backend).
%% API
-export([init_auth/1, authenticate/3, get_permissions/2]).
-export([start_link/1]).

-include("../include/server.hrl").


start_link(Args)->
    ok.
%%%===================================================================
%%% API
%%%===================================================================
-spec init_auth(Args::list(term())) -> ok|{error, Reason::string()}.
init_auth(_Args)->
    ok.


-spec authenticate(Consumer_code::binary(),
                   Room_code::binary(),
                   Auth_info::binary()) -> not_allowed| ok.
authenticate(_Consumer_code, _Room_code, _Auth_info) ->
    ok.


-spec get_permissions(Consumer_Code::binary(),
                      Room_code::binary()) -> permission_type().
get_permissions(_Consumer_code, _Room_code)->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
