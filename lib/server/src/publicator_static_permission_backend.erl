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

has_permission(Permission, Consumer_code, Channel_code, Extra_data,
               #state{filter_list=Filter_list}=State)->
    {lists:any(fun(Filter)->
                       can_pass_filter(Filter,
                                       Permission,
                                       Consumer_code,
                                       Channel_code,
                                       Extra_data)
                   end, Filter_list), State}.



can_pass_filter(#filter{
                   consumer_code=Filter_consumer_code,
                   extra_data=Filter_extra_data,
                   channel_code=Filter_channel_code,
                   can_publish=Filter_can_publish,
                   can_subscribe_messages=Filter_can_subscribe_messages,
                   can_subscribe_all_events=Filter_can_subscribe_all_events,
                   can_create_channel=Filter_can_create_channel
                  }, Permission, Consumer_code, Channel_code, Extra_data)->

    case lists:all(fun({Value,Filter_value})->
                           s_backend_utils:is_equal_or_all(Value,Filter_value)end,
                   [{Consumer_code,Filter_consumer_code},
                    {Channel_code, Filter_channel_code}])
        and s_backend_utils:is_extra_data_passes(Extra_data, Filter_extra_data) of

        true->
            case Permission of
                can_publish-> Filter_can_publish;
                can_subscribe_messages->Filter_can_subscribe_messages;
                can_subscribe_all_events->Filter_can_subscribe_all_events;
                can_create_channel->Filter_can_create_channel
            end;
        false -> false
    end.
