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
-export([init_state/1, authenticate/4]).

-include("../include/server.hrl").

-record(auth_filter, {consumer_code::binary()|all,
                      auth_info::binary()|all,
                      extra_data::list()|all}).

-record(state, {filter_list::list()}).

%%%===================================================================
%%% API
%%%===================================================================
-callback init_state(Auth_args::term()) -> New_state::term().
init_state(Auth_args) ->
    Auth_list = [#auth_filter{consumer_code=proplists:get_value(consumer_code, Auth_arg, all),
                              auth_info=proplists:get_value(auth_info, Auth_arg, all),
                              extra_data=proplists:get_value(extra_data, Auth_arg, [])
                             }
                 || Auth_arg <- Auth_args],
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
                   State::term()) -> denied| granted.
authenticate(Consumer_code, Auth_info, Extra_data, #state{filter_list=Filter_list}=_State) ->
    case lists:any(fun(Filter)->
                           can_authenticate(Filter,
                                            Consumer_code,
                                            Auth_info,
                                            Extra_data)
                   end, Filter_list) of
        true-> granted;
        false-> denied
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec can_authenticate(tuple(), binary(), term(), list()) -> boolean().
can_authenticate(#auth_filter{consumer_code=Filter_consumer_code,
                              auth_info=Filter_auth_info,
                              extra_data=Filter_extra_data},
                 Consumer_code,
                 Auth_info,
                 Extra_data)->

    lists:all(fun({Value,Filter_value})->is_equal_or_all(Value,Filter_value)end,
                   [{Consumer_code,Filter_consumer_code},
                    {Auth_info, Filter_auth_info}])
        and is_extra_data_passes(Extra_data, Filter_extra_data).

%% if Filter value is all or Value equals to filter value return true
is_equal_or_all(_Value, all) -> true;
is_equal_or_all(Value, Filter_value)-> Value ==Filter_value.

%% check if all filter_extra_data exists in Extra_data.
is_extra_data_passes(Extra_data, Filter_extra_data) ->
    is_extra_data_passes(Extra_data, Filter_extra_data, true).

is_extra_data_passes(_Extra_data, _Filter_extra_data, false) -> false;
is_extra_data_passes(_Extra_data, [], true) -> true;
is_extra_data_passes(Extra_data, [{Key, Value}| Filter_extra_data], true)->
    is_extra_data_passes(Extra_data,
                         Filter_extra_data,
                         proplists:get_value(Key, Extra_data, all) == Value).
