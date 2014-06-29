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

-include("../include/server.hrl").

%% API
-export([start_link/3, get/1, get_code/1,
	 get_count/0, stop/1, push_message/3,
	 get_messages/1, subscribe/4,
	 publish/4, get_subscribtions/1, unsubscribe/2,
	 add_message_handler/2, remove_message_handler/2]).

-export([push_add_subscribtion/3]).

-export([push_remove_subscribtion/3]).
-export([push_cached_message/3]).
-export([get_consumers/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TIMEOUT, 1 * 60 * 1000).                % 1 minute

-record(state, {code :: binary(),                           % consumer code
		channels :: dict:dict(binary(), pid()),          % consumer's channel list
		channels_cache :: dict:dict(binary(), pid()),     % channels cache that this consumer reached
		messages :: queue:queue(binary()),          % messages dict (for rest interface)
                max_message_count :: number(),
                current_message_count :: number(),
		handlers :: [pid()],         % current listeners that will received messages
                permission_module :: atom(), % permission_module
                permission_state :: term()   % permission_state
               }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a new consumer server
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary(), atom(), term()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code, Permission_module, Permission_state) ->
    gen_server:start_link(?MODULE, [Code, Permission_module, Permission_state], []).

-spec get(binary()) -> {ok, pid()} | {error, not_found}.
get(Code) ->
    %% check if ets table has given Pid
    %% if it doesn't or value is a dead process
    %% set value to undefined.
    case s_global:get_consumer(Code) of
	undefined -> {error, not_found};
	Pid when is_pid(Pid) -> {ok, Pid}
    end.

-spec get_consumers(Pid::pid(),
                   Channel_code::pid(),
                   Extra_data::term())->
                           {ok, list()}
                               | {error, permission_denied}
                               | {error, invalid_channel_code}.
get_consumers(Pid, Channel_code, Extra_data)->
    gen_server:call(Pid,{get_consumers, Channel_code, Extra_data}).

-spec get_code(pid()) -> {ok, binary()}.
get_code(Pid) ->
    gen_server:call(Pid, get_code).

-spec get_messages(pid()) -> {ok, [tuple()]}.
get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

-spec push_message(pid(), binary(), binary())-> ok.
push_message(Pid, Channel_code, Message) ->
    gen_server:cast(Pid, {push_message, Channel_code, Message}).

-spec push_cached_message(pid(), binary(), binary())-> ok.
push_cached_message(Pid, Channel_code, Message) ->
    gen_server:cast(Pid, {push_cached_message, Channel_code, Message}).

-spec push_add_subscribtion(pid(), binary(), binary())-> ok.
push_add_subscribtion(Pid, Channel_code, Consumer_code) ->
    gen_server:cast(Pid, {push_add_subscribtion, Channel_code, Consumer_code}).

-spec push_remove_subscribtion(pid(), binary(), binary())-> ok.
push_remove_subscribtion(Pid, Channel_code, Consumer_code) ->
    gen_server:cast(Pid, {push_remove_subscribtion, Channel_code, Consumer_code}).


-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_count() -> {ok, integer()}.
get_count() ->
    {ok, ets:info(consumer, size)}.

-spec subscribe(pid(), binary(), channel_handler_type(), term()) ->
                       ok | {error, permission_denied}.
subscribe(Pid, Channel_code, Handler_type, Extra_data)->
    gen_server:call(Pid, {subscribe, Channel_code, Handler_type, Extra_data}).

unsubscribe(Pid, Channel_code)->
    gen_server:call(Pid, {unsubscribe, Channel_code}).

-spec publish(pid(), binary(), binary(), list()) -> ok
                                                        | {error, consumer_not_found}
                                                        | {error, permission_denied}.
publish(Pid, Channel_code, Message, Extra_data)->
    gen_server:call(Pid, {publish, Channel_code, Message, Extra_data}).

-spec get_subscribtions(pid()) -> {ok, dict:dict(binary(), pid())}.
get_subscribtions(Pid) ->
    gen_server:call(Pid, get_subscribtions).

-spec add_message_handler(pid(), pid()) -> ok.
add_message_handler(Pid, Handler_pid) ->
    gen_server:call(Pid, {add_message_handler, Handler_pid}).

-spec remove_message_handler(pid(), pid()) -> ok.
remove_message_handler(Pid,Handler_pid) ->
    gen_server:call(Pid, {remove_message_handler, Handler_pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
init([Code, Permission_module, Permission_state]) ->
    Self = self(),
    case s_global:get_or_register_consumer(Code) of
	Self ->
	    {ok,
	     #state{code=Code,
		    channels=dict:new(),
		    channels_cache=dict:new(),
		    messages=queue:new(),
                    max_message_count=20,
                    current_message_count=0,
		    handlers=[],
                    permission_module=Permission_module,
                    permission_state=Permission_state},
	     ?TIMEOUT
	    };
	Pid when is_pid(Pid) -> 
	    {error, {already_exists, Pid}}
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
    {reply, Reply, State, ?TIMEOUT};

handle_call({publish, Channel_code, Message, Extra_data}, _From,
            #state{code=Code,
                   permission_module=Permission_module,
                   permission_state=Permission_state}=State) ->
    case get_cached_channel(Channel_code, State, Extra_data) of
        {{ok, Channel_pid}, State2} ->
            case Permission_module:has_permission(can_publish,
                                                  Code,
                                                  Channel_code,
                                                  Extra_data,
                                                  Permission_state) of
                {true,Permission_state2} ->
                    s_channel:publish(Channel_pid, Message),
                    {reply,
                     ok,
                     State2#state{permission_state=Permission_state2},
                     ?TIMEOUT};
                {false,Permission_state2} ->
                    {reply,
                     {error, permission_denied},
                     State2#state{permission_state=Permission_state2},
                     ?TIMEOUT}
            end;
        {{error, permission_denied}, State2} ->
            {reply, {error, permission_denied}, State2, ?TIMEOUT}
    end;

handle_call({subscribe, Channel_code, Handler_type, Extra_data}, _From,
	    #state{code=Code,
		   channels=Channels_dict,
                   permission_module=Permission_module,
                   permission_state=Permission_state}=State) ->
    case get_cached_channel(Channel_code, State, Extra_data) of
        {{ok, Channel_pid}, State2} ->
            Reply = ok,
            %% if value is already exist in the dictionary log a warning
            case dict:is_key(Channel_code, Channels_dict) of
                true ->
                    {reply, Reply, State, ?TIMEOUT};
                false ->
                    case Permission_module:has_permission(
                           case Handler_type of
                               all -> can_subscribe_all_events;
                               message_only -> can_subscribe_messages
                           end,
                           Code,
                           Channel_code,
                           Extra_data,
                           Permission_state) of
                        {true, Permission_state2} ->
                            ok = s_channel:add_consumer(Channel_pid, self(), Code, Handler_type),
                            {reply,
                             Reply,
                             State2#state{channels=dict:store(Channel_code,
                                                              Channel_pid,
                                                              Channels_dict),
                                          permission_state=Permission_state2},
                             ?TIMEOUT};

                        {false, Permission_state2} ->
                            {reply,
                             {error, permission_denied},
                             State2#state{permission_state=Permission_state2},
                             ?TIMEOUT}
                    end
            end;
        {{error, permission_denied}, State2} ->
            {reply, {error, permission_denied}, State2}
    end;

handle_call({unsubscribe, Channel_code}, _From,
	    #state{channels=Channels_dict,
		   code=Code}=State) ->

    Reply = ok,
    %% if value is already exist in the dictionary log a warning
    case dict:find(Channel_code, Channels_dict) of
	{ok, Channel_pid} ->
	    Channels_dict2 = dict:erase(Channel_code, Channels_dict),
	    ok = s_channel:remove_consumer(Channel_pid, Code);
	error ->
	    Channels_dict2 = Channels_dict
    end,
    {reply, Reply, State#state{channels=Channels_dict2}, ?TIMEOUT};


handle_call({add_message_handler, Handler_pid}, _From,
	    #state{handlers=Handler_list}=State) ->
    case lists:member(Handler_pid, Handler_list) of
	true -> New_handler_list = Handler_list;
	false -> New_handler_list = [Handler_pid | Handler_list]
    end,
    Reply = ok,
    {reply, Reply, State#state{handlers=New_handler_list}, ?TIMEOUT};

handle_call({remove_message_handler, Handler_pid}, _From,
	    #state{handlers=Handler_list}=State) ->
    Reply = ok,
    {reply, Reply, State#state{handlers=lists:delete(Handler_pid, Handler_list)}, ?TIMEOUT};


%% Gets all messages for this user and returns them
handle_call(get_messages, _From, #state{messages=Messages_queue}=State) ->
    Reply = {ok, queue:to_list(Messages_queue)},
    {reply, Reply, State#state{messages=queue:new(),
                               current_message_count=0}, ?TIMEOUT};

handle_call(get_subscribtions, _From, #state{channels=Channels_dict}=State)->
    Reply = {ok, dict:fetch_keys(Channels_dict)},
    {reply,Reply, State, ?TIMEOUT};


handle_call({get_consumers,
             Channel_code,
             Extra_data}, _From, State)->
    case get_cached_channel(Channel_code, State, Extra_data) of
        {{ok, Channel_pid}, State2} ->
            {ok, Consumer_list} = s_channel:get_consumers(Channel_pid),
            {reply, {ok, Consumer_list}, State2, ?TIMEOUT};
        {{error, permission_denied}, State2} ->
            {reply, {error, permission_denied}, State2, ?TIMEOUT}
    end.


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
handle_cast({push_message, Channel_code, Message}, State) ->
    handle_cast({push, message, Channel_code, Message}, State);

handle_cast({push_cached_message, Channel_code, Message}, State) ->
    handle_cast({push, cached_message, Channel_code, Message}, State);

handle_cast({push_add_subscribtion, Channel_code, Consumer_code}, State) ->
    handle_cast({push, add_subscribtion, Channel_code, Consumer_code}, State);

handle_cast({push_remove_subscribtion, Channel_code, Consumer_code}, State) ->
    handle_cast({push, remove_subscribtion, Channel_code, Consumer_code}, State);

handle_cast({push, Message_type, Channel_code, Msg},
	    #state{messages=Message_queue,
                   max_message_count=Max_message_count,
                   current_message_count=Current_message_count,
                   handlers=[]}=State) ->
    
    Message = #message{type=Message_type,
                       data=Msg,
                       channel_code=Channel_code},

    if
        Max_message_count =< Current_message_count ->
            Message_queue1 = queue:drop(queue:in(Message,Message_queue)),
            Current_message_count1 = Current_message_count;
        Max_message_count > Current_message_count ->
            Message_queue1 = queue:in(Message,Message_queue),
            Current_message_count1 = Current_message_count + 1
    end,
    
    {noreply, State#state{messages=Message_queue1,
                          current_message_count=Current_message_count1},
     ?TIMEOUT};

handle_cast({push, Message_type, Channel_code, Msg},
	    #state{handlers=Handler_list}=State) ->
    Alive_handler_list = lists:filter(fun is_process_alive/1, Handler_list),
    New_state = State#state{handlers=Alive_handler_list},
    case Alive_handler_list of
	[] ->
	    lager:info("All hadlers are dead, Switch to buffer mode"),
	    handle_cast({push, Message_type, Channel_code, Msg}, New_state);
	_ ->
            Message = #message{type=Message_type,
                               data=Msg,
                               channel_code=Channel_code},

            lists:foreach(fun(Pid)->
				   Pid ! Message
                          end, Handler_list),
	     {noreply, New_state, ?TIMEOUT}
	end;

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
handle_info(timeout, #state{code=Code,
                            handlers=Handler_list}=State)->
    lager:info("Consumer ~p has timeout", [Code]),
    Alive_handler_list = lists:filter(fun is_process_alive/1, Handler_list),
    case Alive_handler_list of
	[] ->
	    lager:info("~p - All handlers are dead. Consumer is dying.", [Code]),
            {stop, normal , State};
	_ ->
            lager:info("~p - There is still alive handlers. Consumer will stay alive",
                       [Code]),
            {noreply, State, ?TIMEOUT}
	end;

handle_info(Info, #state{code=Code}=State) ->
    lager:warning("Unhandled info message in consumer ~p (~p)", [Code, Info]),
    {noreply, State, ?TIMEOUT}.

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
terminate(Reason, #state{code=Code,
                         channels=Channel_dict}=_State) ->
    dict:fold(fun(_Channel_code, Pid, ok)->
                      s_channel:remove_consumer(Pid, Code),
                      ok end, ok, Channel_dict),

    lager:debug("=============================================================="),
    lager:info("Terminate consumer ~p (~p)", [Code, Reason]),
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
get_cached_channel(Channel_code,
                   #state{channels=Channels_dict,
                          channels_cache=Channels_cache_dict}=State,
                  Extra_data) ->
    case dict:find(Channel_code, Channels_dict) of
	{ok, Channel_pid} -> Result = Channel_pid,
			     {{ok, Result}, State};
	error ->
	    case dict:find(Channel_code, Channels_cache_dict) of
		{ok, Cached_channel_pid} -> Result = Cached_channel_pid,
					    {{ok, Result}, State};
		error ->
                    %% result is not in Channel_dict nor Channel_cache_dict
                    case get_or_create_channel(Channel_code,
                                               State,
                                               Extra_data) of
                        {{ok, New_channel_pid}, State2} ->
                            State3 = State2#state{channels_cache=
                                                      dict:store(Channel_code,
                                                                 New_channel_pid,
                                                                 Channels_cache_dict)},
                            {{ok, New_channel_pid}, State3};
                        {{error, permission_denied}, State2} ->
                            {{error, permission_denied}, State2}
                    end
	    end
    end.


get_or_create_channel(Channel_code,
                      #state{code=Code,
                             permission_module=Permission_module,
                             permission_state=Permission_state}=State,
                     Extra_data)->
    case s_global:get_channel(Channel_code) of
        undefined ->
            case Permission_module:has_permission(can_create_channel,
                                                  Code,
                                                  Channel_code,
                                                  Extra_data,
                                                  Permission_state) of
                {true, Permission_state2} ->
                    Result = s_channel_sup:start_child(Channel_code),
                    State2 = State#state{permission_state=Permission_state2};
                {false, Permission_state2} ->
                    Result = {error, permission_denied},
                    State2 = State#state{permission_state=Permission_state2}
            end;
        Pid when is_pid(Pid) ->
            Result = {ok, Pid},
            State2 = State
    end,
    {Result, State2}.
