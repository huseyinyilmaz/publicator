-module(publicator_static_permission_backend).
-behivour(s_permission_backend).
%% API
-export([init_state/1]).
-export([has_permission/5]).
-include("../include/server.hrl").


-record(filter, {consumer_code::binary()|all,
                 extra_data::list(),
                 channel_code::binary()|all,
                 can_publish::boolean(),
                 can_subscribe_messages::boolean(),
                 can_subscribe_all_events::boolean(),
                 can_create_channel::boolean()}).

-record(state, {filter_list::list()}).





%%%===================================================================
%%% API
%%%===================================================================
-callback init_state(Auth_args::term()) -> New_state::term().
init_state(Args) ->
    Filter_list = [#filter{consumer_code=proplists:get_value(consumer_code, Arg, all),
                           channel_code=proplists:get_value(channel_code, Arg, all),
                           extra_data=proplists:get_value(extra_data, Arg, []),
                           can_publish=proplists:get_value(can_publish, Arg, false),
                           can_subscribe_messages=proplists:get_value(can_subscribe_messages, Arg, false),
                           can_subscribe_all_events=proplists:get_value(can_subscribe_all_events, Arg, false),
                           can_create_channel=proplists:get_value(can_create_channel, Arg, false)}
                   || Arg <- Args],
    #state{filter_list=Filter_list}.


-spec has_permission(Permission::atom(),
                     Consumer_Code::binary(),
                     Channel_code::binary(),
                     Extra_data::term(),
                     State::term()) -> {Result::boolean(), New_State::term()}.
has_permission(can_create_channel, _Consumer_code, _Chanel_code, _Extra_data, _State)->
    lager:debug("permission backend can_create_channel"),
    {true, _State};
has_permission(can_publish, _Consumer_code, _Chanel_code, _Extra_data, _State)->
    lager:debug("permission backend can_publish"),
    {true, _State};
has_permission(can_subscribe_messages, _Consumer_code, _Chanel_code, _Extra_data, _State)->
    lager:debug("permission backend can_subscribe_messages"),
    {true, _State};
has_permission(can_subscribe_all_events, _Consumer_code, _Chanel_code, _Extra_data, _State)->
    lager:debug("permission backend can_subscribe_all_events"),
    {true, _State};
has_permission(_Permission, _Consumer_code, _Chanel_code, _Extra_data, _State)->
    {true, _State}.
