%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(publicator_static_auth_backend).
-behivour(s_auth_backend).
%% API
-export([init_state/1, authenticate/4, get_permissions/4]).

-include("../include/server.hrl").

-record(auth_filter, {consumer_code::binary()|all,
                      group::atom(),
                      auth_info::binary()|all}).

-record(state, {filter_list::list()}).

%%%===================================================================
%%% API
%%%===================================================================
-callback init_state(Auth_args::term()) -> New_state::term().
init_state(Auth_args) ->
    Auth_list = [#auth_filter{consumer_code=proplists:get_value(consumer_code, Auth_args, all),
                              group=proplists:get_value(group, Auth_args, all),
                              auth_info=proplists:get_value(consumer_code, Auth_args, all)}],

    #state{filter_list=Auth_list}.



%% -spec init([Args::term()]) -> {ok, State::term()}.
%% init([Args])->
%%     %% lager:debug("==================="),
%%     %% lager:debug("Start auth  backend"),
%%     lager:debug("///////////////////////////"),
%%     lager:debug("call s_authbackend:start_link/1 "),
%%     lager:debug("Args=~p~n", [Args]),
%%     Auth_list = [#auth_filter{consumer_code=proplists:get_value(consumer_code, Arg, all),
%%                               group=proplists:get_value(group, Arg, all),
%%                               auth_info=proplists:get_value(consumer_code, Arg, all)}
%%                  || Arg <- Args],
%%     {ok, #state{filter_list=Auth_list}}.


-spec authenticate(Consumer_code::binary(),
                          Auth_info::binary(),
                          Extra_data::term(),
                          State::term()) -> denied| ok.
authenticate(Consumer_code, Auth_info, Extra_data, State) ->
    lager:debug("=============--------------================"),
    lager:info("~p,~p,~p,~p", [Consumer_code, Auth_info, Extra_data, State]),
    granted.
    %% denied.

-spec get_permissions(Consumer_Code::binary(),
                      Room_code::binary(),
                      Extra_data::term(),
                      State::term()) -> permission_type().
get_permissions(_Consumer_code, _Room_code, _Extra_data, _State)->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
