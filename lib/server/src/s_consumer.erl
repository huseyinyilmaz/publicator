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
	 get_messages/2, get_messages/1, subscribe/4,
	 publish/4, get_subscribtions/1, unsubscribe/2,
	 add_message_handler/2, remove_message_handler/2]).

-export([push_add_subscribtion/3]).

-export([push_remove_subscribtion/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TIMEOUT, 2 * 60 * 1000). % two minutes

-record(state, {code :: binary(), % consumer code
		channels :: dict(), % consumer's channel list
		channels_cache ::dict(), % channels cache that this consumer reached
		messages :: dict(), % messages that this consumer got. (for rest interface)
		handlers :: [pid()], % current listeners that will received messages
                permission_module :: atom(), % permission_module
                permission_state :: term() % permission_state
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

-spec subscribe(pid(), binary(), channel_handler_type(), term()) -> ok.
subscribe(Pid, Channel_code, Handler_type, Extra_data)->
    gen_server:call(Pid, {subscribe, Channel_code, Handler_type, Extra_data}).

unsubscribe(Pid, Channel_code)->
    gen_server:call(Pid, {unsubscribe, Channel_code}).

-spec publish(pid(), binary(), binary(), list()) -> ok.
publish(Pid, Channel_code, Message, Extra_data)->
    gen_server:call(Pid, {publish, Channel_code, Message, Extra_data}).

-spec get_subscribtions(pid()) -> {ok, dict()}.
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
		    messages=dict:new(),
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

handle_call({publish, Channel_code, Message, Extra_data}, _From, State) ->
    {ok, Channel_pid, State2} = get_cached_channel(Channel_code, State, Extra_data),
    case get_cached_channel(Channel_code, State, Extra_data) of
        {{ok, Channel_pid}, State2} ->
            s_channel:publish(Channel_pid, Message),
            {reply, ok, State2, ?TIMEOUT};
        {{error, permission_denied}, State2} ->
            {reply, {error, permission_denied}, State2, ?TIMEOUT}
    end;

handle_call({subscribe, Channel_code, Handler_type, Extra_data}, _From,
	    #state{code=Code,
		   channels=Channels_dict}=State) ->
    case get_cached_channel(Channel_code, State, Extra_data) of
        {{ok, Channel_pid}, State2} ->
            Reply = ok,
            %% if value is already exist in the dictionary log a warning
            case dict:is_key(Channel_code, Channels_dict) of
                true ->
                    {reply, Reply, State, ?TIMEOUT};
                false ->
                    ok = s_channel:add_consumer(Channel_pid, self(), Code, Handler_type, Extra_data),
                    {reply, Reply, State2#state{channels=dict:store(Channel_code,
                                                                    Channel_pid,
                                                                    Channels_dict)},
                     ?TIMEOUT}
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




%% Gets all messages for this user
handle_call({get_messages, Channel_code}, _From, #state{messages=Messages_dict}=State) ->
    Messages = case dict:find(Channel_code, Messages_dict) of
		   error -> [];
		   {ok, Result} -> Result
	       end,
    Messages_dict2 = dict:erase(Channel_code, Messages_dict),
    %% Messages is type of {Type, Message} we should return this properly
    Reply = {ok, Messages},
    {reply, Reply, State#state{messages=Messages_dict2},
    ?TIMEOUT};

%% Gets all messages for this user and returns them
handle_call(get_messages, _From, #state{messages=Messages_dict}=State) ->
    %% Messages = lists:map(fun({_Key, Value}) -> Value end,
    %% 			 dict:to_list(Messages_dict)),
    Messages = Messages_dict,
    Reply = {ok, Messages},
    {reply, Reply, State#state{messages=dict:new()}, ?TIMEOUT};

handle_call(get_subscribtions, _From, #state{channels=Channels_dict}=State)->
    Reply = {ok, dict:fetch_keys(Channels_dict)},
    {reply,Reply, State, ?TIMEOUT}.

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

handle_cast({push_add_subscribtion, Channel_code, Consumer_code}, State) ->
    handle_cast({push, add_subscribtion, Channel_code, Consumer_code}, State);

handle_cast({push_remove_subscribtion, Channel_code, Consumer_code}, State) ->
    handle_cast({push, remove_subscribtion, Channel_code, Consumer_code}, State);

handle_cast({push, Message_type, Channel_code, Message},
	    #state{messages=Messages_dict, handlers=[]}=State) ->
    {noreply, State#state{messages=dict:append(Channel_code, {Message_type, Message},
					       Messages_dict)},
     ?TIMEOUT};

handle_cast({push, Message_type, Channel_code, Message},
	    #state{handlers=Handler_list}=State) ->
    Alive_handler_list = lists:filter(fun is_process_alive/1, Handler_list),
    New_state = State#state{handlers=Alive_handler_list},
    case Alive_handler_list of
	[] ->
	    lager:info("All hadlers are dead, Switch to buffer mode"),
	    handle_cast({push, Message_type, Channel_code, Message}, New_state);
	_ -> lists:foreach(fun(Pid)->
				   Pid ! {Message_type, Channel_code, Message}
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

    lager:info("=============================================================="),
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
			     {ok, Result, State};
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
