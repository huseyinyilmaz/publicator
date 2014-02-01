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

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback init_state(Auth_args::term()) -> New_state::term().



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback authenticate(Consumer_Code::binary(),
                       Auth_info::binary(),
                       State::term()) -> denied| granted.


%%%===================================================================
%%% API
%%%===================================================================
-spec get_authentication_backend()->{Module::atom(), Configuration::term()}.
get_authentication_backend() ->
    {ok, {Module, Args}} = application:get_env(server, auth_backend),
    State = Module:init_state(Args),
    {Module, State}.
