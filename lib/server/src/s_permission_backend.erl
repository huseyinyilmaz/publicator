%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Behivour that implements authentication backend behivour.
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(s_permission_backend).

-include("../include/server.hrl").

-export([get_permission_backend/0]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback init_state(Args::term()) -> State::term().

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback get_permissions(Consumer_Code::binary(),
                          Channel_code::binary(),
                          State::term()) -> permission_type().


%%%===================================================================
%%% API
%%%===================================================================
-spec get_permission_backend()->{Module::atom(), Configuration::term()}.
get_permission_backend() ->
    {ok, {Module, Args}} = application:get_env(server, permission_backend),
    State = Module:init_state(Args),
    {Module, State}.
