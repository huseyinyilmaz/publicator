%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(s_consumer).

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, get/1, get_code/1,
	 get_count/0, stop/1, push_message/3,
	 get_messages/2, get_messages/1, subscribe/2,
	 publish/3, get_subscribtions/1, unsubscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code :: binary(),
		channels :: dict(),
		channels_cache ::dict(),
		messages :: dict()}).

%% -include("c_room_event.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a new consumer server
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code) ->
    error_logger:info_report({consumer_start_link, Code}),
    gen_server:start_link(?SERVER, [Code], []).

-spec get(binary()) -> {ok, pid()} | {error, not_found}.
get(Code) ->
    %% check if ets table has given Pid
    %% if it doesn't or value is a dead process
    %% set value to undefined.
    case ets:lookup(consumers, Code) of
	[{Code, Pid}] ->
	    %% if process is dead remove it and
	    %% return not found
	    case is_process_alive(Pid) of
		true -> {ok, Pid};
		false -> ets:delete(consumers, Code),
			 {error, not_found}
	    end;
	    [] -> {error, not_found}
	end.


-spec get_code(pid()) -> {ok, binary()}.
get_code(Pid) ->
    gen_server:call(Pid, get_code).

-spec get_messages(pid()) -> {ok, dict()}.
get_messages(Pid) ->
    gen_server:call(Pid, get_messages).


-spec get_messages(pid(), binary()) -> {ok, [binary()]}.
get_messages(Pid, Channel_code) ->
    gen_server:call(Pid, {get_messages, Channel_code}).


-spec push_message(pid(), binary(), binary())-> ok.
push_message(Pid, Channel_code, Message) ->
    gen_server:cast(Pid, {push_message, Channel_code, Message}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_count() -> {ok, integer()}.
get_count() ->
    {ok, ets:info(consumer, size)}.

subscribe(Pid, Channel_code)->
    gen_server:cast(Pid, {subscribe, Channel_code}).

unsubscribe(Pid, Channel_code)->
    gen_server:cast(Pid, {unsubscribe, Channel_code}).


publish(Pid, Channel_code, Message)->
    gen_server:cast(Pid, {publish, Channel_code, Message}).

get_subscribtions(Pid) ->
    gen_server:call(Pid, get_subscribtions).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Code]) ->
    case s_consumer:get(Code) of
	{ok , _} -> {stop, already_exists};
	{error, not_found} ->
	    ets:insert(consumers, {Code, self()}),
	    {ok, #state{code=Code,
			channels=dict:new(),
			channels_cache=dict:new(),
			messages=dict:new()}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_code, _From, #state{code=Code}=State) ->
    Reply = {ok, Code},
    {reply, Reply, State};

%% Gets all messages for this user
handle_call({get_messages, Channel_code}, _From, #state{messages=Messages_dict}=State) ->
    Messages = case dict:find(Channel_code, Messages_dict) of
		   error -> [];
		   {ok, Result} -> Result
	       end,
    Messages_dict2 = dict:erase(Channel_code, Messages_dict),
    error_logger:info_report({get_messages__channel_code, Messages}),
    Reply = {ok, Messages},
    {reply, Reply, State#state{messages=Messages_dict2}};

%% Gets all messages for this user and returns them
handle_call(get_messages, _From, #state{messages=Messages_dict}=State) ->
    %% Messages = lists:map(fun({_Key, Value}) -> Value end,
    %% 			 dict:to_list(Messages_dict)),
    Messages = Messages_dict,
    Reply = {ok, Messages},
    {reply, Reply, State#state{messages=dict:new()}};

handle_call(get_subscribtions, _From, #state{channels=Channels_dict}=State)->
    Reply = {ok, dict:fetch_keys(Channels_dict)},
    {reply,Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({push_message, Channel_code, Message},
	    #state{messages=Messages_dict}=State) ->
    error_logger:info_report({message_received, Channel_code, Message}),
    {noreply, State#state{messages=dict:append(Channel_code, Message, Messages_dict)}};

handle_cast({subscribe, Channel_code},
	    #state{channels=Channels_dict}=State) ->
    
    {ok, Channel_pid, State2} = get_cached_channel(Channel_code, State),
    %% if value is already exist in the dictionary log a warning
    case dict:is_key(Channel_code, Channels_dict) of
	true ->
	    {noreply,State};
	false ->
	    ok = s_channel:add_handler(Channel_pid, Channel_code, self()),
	    error_logger:info_report({s_consumer__handle_cast__subscribe, Channel_code}),
	    {noreply, State2#state{channels=dict:store(Channel_code,
					       Channel_pid,
					       Channels_dict)}}
    end;

handle_cast({publish, Channel_code, Message}, State) ->
    {ok, Channel_pid, State2} = get_cached_channel(Channel_code, State),
    s_channel:publish(Channel_pid, self(), Message),
    {noreply, State2};

handle_cast({unsubscribe, Channel_code},
	    #state{channels=Channels_dict}=State) ->
    
    %% if value is already exist in the dictionary log a warning
    case dict:find(Channel_code, Channels_dict) of
	{ok, Channel_pid} ->
	    Channels_dict2 = dict:erase(Channel_code, Channels_dict),
	    ok = s_channel:delete_handler(Channel_pid, self());
	error ->
	    Channels_dict2 = Channels_dict
    end,
    error_logger:info_report({s_consumer__handle_cast__subscribe, Channel_code}),
    {noreply, State#state{channels=Channels_dict2}};

handle_cast(stop, State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Gets Channel from subscribtions list or channels cache,
%%% If it cannot find it, function fill query the global channel list
get_cached_channel(Channel_code, #state{channels=Channels_dict,
					channels_cache=Channels_cache_dict}=State) ->
    case dict:find(Channel_code, Channels_dict) of
	{ok, Channel_pid} -> Result = Channel_pid,
			     {ok, Result, State};
	error ->
	    case dict:find(Channel_code, Channels_cache_dict) of
		{ok, Cached_channel_pid} -> Result = Cached_channel_pid,
					    {ok, Result, State};
		error ->
		    %% result is not in Channel_dict nor Channel_cache_dict
		    {ok, New_channel_pid}
			= s_manager:get_or_create_channel(Channel_code),
		    State2 = State#state{channels_cache=
					     dict:store(Channel_code,
							New_channel_pid,
							Channels_cache_dict)},
		    {ok, New_channel_pid, State2}
	    end
    end.

    
