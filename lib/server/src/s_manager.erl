%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Library that manages ets
%%% @end
%%% Created : 13 Aug 2013 by Huseyin Yilmaz <huseyin@saturn.local>
%%%-------------------------------------------------------------------
-module(s_manager).

%% API
-export([init/0, destroy/0]).
-export([get_channel/1, get_consumer/1, get_channels/0,
	 get_subscribtions/1, add_channel/2, add_consumer/2, bind/2, unbind/2]).
%%%===================================================================
%%% API
%%%===================================================================
init()->
    ets:new(channel_to_consumer,[bag, public, named_table]),
    ets:new(consumer_to_channel,[bag, public, named_table]),
    ets:new(channels,[set, public, named_table]),
    ets:new(consumers,[set, public, named_table]),
    error_logger:info_report({s_manager__init, "Ets table initialization complete"}).

destroy()->
    ets:delete(channel_to_consumer),
    ets:delete(consumer_to_channel),
    ets:delete(channels),
    ets:delete(consumers),
    error_logger:info_report({s_manager__destroy, "Ets table removal complete"}).
    
get_channels()->
    ets:foldl(fun({Key,_Value},Acc) -> [Key| Acc] end, [], channels).

get_subscribtions(Consumer_code) ->
    lists:map(fun({_Key, Value}) -> Value end,
	      ets:lookup(consumer_to_channel,Consumer_code)).

get_channel(Channel_code) ->
    %% XXX if PID is dead, remove it and return not_found
    error_logger:info_report({get_channel_before_ets_lookup, Channel_code}),    
    Val = ets:lookup(channels, Channel_code),
    error_logger:info_report({get_channel_after_ets_lookup, Channel_code, Val}),    
    case ets:lookup(channels, Channel_code) of
	[] -> {error, not_found};
	%% {ok, Channel_pid} = s_channel_sup:start_child(Channel_code);
	[{Channel_code, Pid}] -> {ok, Pid}
    end.

add_channel(Channel_code, Channel_pid) ->
    ets:insert(channels,[{Channel_code, Channel_pid}]).

get_consumer(Consumer_code) ->
    case ets:lookup(consumers, Consumer_code) of
	[] -> {error, not_found};
	[{Consumer_code, Pid}] -> {ok, Pid}
    end.

add_consumer(Consumer_code, Consumer_pid) ->
    ets:insert(consumers,[{Consumer_code, Consumer_pid}]).
    
bind(Channel_code, Consumer_code) ->
    error_logger:info_report({s_manager__bind, Channel_code, Consumer_code}),
    ets:insert(channel_to_consumer,{Channel_code, Consumer_code}),
    ets:insert(consumer_to_channel,{Consumer_code, Channel_code}),
    ok.

unbind(Channel_code, Consumer_code) ->
    error_logger:info_report({s_manager__unbind, Channel_code, Consumer_code}),
    ets:delete_object(channel_to_consumer,{Channel_code, Consumer_code}),
    ets:delete_object(consumer_to_channel,{Consumer_code, Channel_code}),
    ok.
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
