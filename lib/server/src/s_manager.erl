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
	 add_channel/2, add_consumer/2, get_or_create_channel/1, create_consumer/0]).
%%%===================================================================
%%% API
%%%===================================================================
init()->
    channels = ets:new(channels,[set, public, named_table]),
    consumers = ets:new(consumers,[set, public, named_table]).

destroy()->
    true = ets:delete(channels),
    true = ets:delete(consumers),
    ok.

get_channels()->
    ets:foldl(fun({Key,_Value},Acc) -> [Key| Acc] end, [], channels).


get_channel(Channel_code) ->
    %% XXX if PID is dead, remove it and return not_found
    case ets:lookup(channels, Channel_code) of
	[] -> {error, not_found};
	%% {ok, Channel_pid} = s_channel_sup:start_child(Channel_code);
	[{Channel_code, Pid}] -> {ok, Pid}
    end.

add_channel(Channel_code, Channel_pid) ->
    case get_channel(Channel_code) of
	{error, not_found} ->
	    ets:insert(channels,[{Channel_code, Channel_pid}]);
	{ok, Channel_pid} -> ok;
	{ok, Org_Channel_pid} ->
	    {error, already_exists, Channel_code, Org_Channel_pid}
    end.
	    

get_consumer(Consumer_code) ->
    case ets:lookup(consumers, Consumer_code) of
	[] -> {error, not_found};
	[{Consumer_code, Pid}] -> {ok, Pid}
    end.

add_consumer(Consumer_code, Consumer_pid) ->
    case get_consumer(Consumer_code) of
	{error, not_found} ->
	    ets:insert(consumers,[{Consumer_code, Consumer_pid}]);
	{ok, Consumer_pid} -> ok;
	{ok, Org_Consumer_pid} ->
	    {error, already_exists, Consumer_code, Org_Consumer_pid}
    end.

    

get_or_create_channel(Channel_code)->
    case get_channel(Channel_code) of
	{error, not_found} -> {ok, New_channel_pid} = s_channel_sup:start_child(),
			      case add_channel(Channel_code, New_channel_pid) of
				  {error, already_exists, Channel_code, Org_channel_pid} ->
				      s_channel:stop(New_channel_pid),
				      Channel_pid = Org_channel_pid;
				  _ -> Channel_pid = New_channel_pid
				  end;
	{ok, Pid} -> Channel_pid = Pid
    end,
    {ok, Channel_pid}.

%% Creates a new consumer and returns its code and pid number
-spec create_consumer() -> {ok, binary(), pid()}.
create_consumer() ->
    Consumer_code = s_utils:generate_code(),
    case get_consumer(Consumer_code) of
	{ok, _Pid} -> create_consumer();
	{error, not_found} -> {ok, New_consumer_pid} = s_consumer_sup:start_child(Consumer_code),
			      case add_consumer(Consumer_code, New_consumer_pid) of
				  {error, already_exists, Consumer_code, _Org_consumer_pid} ->
				      s_consumer:stop(New_consumer_pid),
				      create_consumer();
				  _ -> {ok, Consumer_code, New_consumer_pid}
			      end
			      
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
