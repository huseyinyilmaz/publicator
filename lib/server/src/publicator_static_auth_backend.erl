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
-export([init/1, handle_authenticate/4, handle_permissions/4]).

-include("../include/server.hrl").

-record(auth_filter, {consumer_code::binary()|all,
                      group::atom(),
                      auth_info::binary()|all}).

-record(state, {filter_list::list()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec init([Args::term()]) -> {ok, State::term()}.
init([Args])->
    %% lager:debug("==================="),
    %% lager:debug("Start auth  backend"),
    lager:debug("///////////////////////////"),
    lager:debug("call s_authbackend:start_link/1 "),
    lager:debug("Args=~p~n", [Args]),
    Auth_list = [#auth_filter{consumer_code=proplists:get_value(consumer_code, Arg, all),
                              group=proplists:get_value(group, Arg, all),
                              auth_info=proplists:get_value(consumer_code, Arg, all)}
                 || Arg <- Args],
    {ok, #state{filter_list=Auth_list}}.


-spec handle_authenticate(Consumer_code::binary(),
                          Auth_info::binary(),
                          Extra_data::term(),
                          State::term()) -> not_allowed| ok.
handle_authenticate(_Consumer_code, _Auth_info, _Extra_data, _State) ->
    ok.


-spec handle_permissions(Consumer_Code::binary(),
                         Room_code::binary(),
                         Extra_data::term(),
                         State::term()) -> permission_type().
handle_permissions(_Consumer_code, _Room_code, _Extra_data, _State)->
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
