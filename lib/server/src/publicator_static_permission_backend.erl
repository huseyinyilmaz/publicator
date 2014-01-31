-module(publicator_static_permission_backend).
-behivour(s_permission_backend).
%% API
-export([init_state/1, get_permissions/4]).
-export([has_permission/2]).
-include("../include/server.hrl").


-record(filter, {consumer_code::binary()|all,
                 extra_data::list(),
                 channel_code::binary()|all,
                 can_publish::boolean(),
                 can_subscribe::boolean(),
                 can_subscribe_all_events::boolean(),
                 can_create_channel::boolean()}).

-record(state, {filter_list::list()}).





%%%===================================================================
%%% API
%%%===================================================================
-callback init_state(Auth_args::term()) -> New_state::term().
init_state(Args) ->
    Filter_list = [#filter{consumer_code=proplists:get_value(consumer_code, Arg, all),
                           extra_data=proplists:get_value(extra_data, Arg, []),
                           channel_code=proplists:get_value(channel_code, Arg, all),
                           can_publish=proplists:get_value(can_publish, Arg, all),
                           can_subscribe=proplists:get_value(can_subscribe, Arg, all),
                           can_subscribe_all_events=proplists:get_value(can_subscribe_all_events, Arg, all),
                           can_create_channel=proplists:get_value(can_create_channel, Arg, all)}
                   || Arg <- Args],
    #state{filter_list=Filter_list}.


-spec get_permissions(Consumer_Code::binary(),
                      Room_code::binary(),
                      Extra_data::term(),
                      State::term()) -> permission_type().
get_permissions(_Consumer_code, _Room_code, _Extra_data, _State)->
    ok.


has_permission(Permissions, Permission_name) ->
    proplists:get_value(Permission_name, Permissions, false).
