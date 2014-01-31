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

-record(auth_filter, {consumer_code::binary()|all,
                      auth_info::binary()|all,
                      extra_data::list()|all}).

-record(state, {filter_list::list()}).


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
    lager:debug("asdfasdfasdfasdf1234123412341324"),
    lager:debug("~p~n", [application:get_env(server, auth_backend)]),
    {ok, {Module, Args}} = application:get_env(server, auth_backend),
    {Module, Args}.