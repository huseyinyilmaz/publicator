%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Behivour that implements authentication backend behivour.
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(s_auth_backend).

-include("../include/server.hrl").

-export([get_authentication_backend/0]).

-define(DEFAULT_AUTH_BACKEND, {publicator_static_auth_backend,
                               [[{consumer_code, all},
                                 {group, all},
                                 {auth_info, all}]]}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback init([Args::term()]) -> {ok, State::term()}|
                                  {ok, State::term(), Timeout::integer()|infinity}|
                                  ignore|
                                  {stop, Reason::term()}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback handle_authenticate(Consumer_Code::binary(),
                              Auth_info::binary(),
                              State::term()) -> not_allowed| ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback handle_permissions(Consumer_Code::binary(),
                             Room_code::binary(),
                             State::term()) -> permission_type().

%%%===================================================================
%%% API
%%%===================================================================
-spec get_authentication_backend()->{Module::atom(), Configuration::term()}.
get_authentication_backend() ->
    {_Module, _Args} = s_utils:get_env(server,
                                     auth_backend,
                                     ?DEFAULT_AUTH_BACKEND).
