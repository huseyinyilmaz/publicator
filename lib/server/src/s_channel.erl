%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(s_channel).

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([publish/2]).
-export([get_consumers/1]).
-export([add_consumer/4]).
-export([remove_consumer/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code :: binary(),
		consumer_table :: ets:tid(),
                cache_size :: number(),
                current_cache_size :: number(),
                cache :: queue:queue(binary()),
                timeout :: number()|infinity}).

-include("../include/server.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Code, Cache_size, Timeout) ->
    gen_server:start_link(?MODULE, [Code, Cache_size, Timeout], []).


publish(Channel_pid, Message) ->
    gen_server:cast(Channel_pid, {publish, Message}).


%spec get_consumers(Channel_pid::binary()) -> {ok, [pid()]}.
get_consumers(Channel_pid) ->
    {ok, Consumer_list} = gen_server:call(Channel_pid, get_consumers),
    {ok, Consumer_list}.

add_consumer(Channel_pid, Consumer_pid, Consumer_code, Handler_type) ->
    gen_server:call(Channel_pid, {add_consumer, Consumer_pid, Consumer_code, Handler_type}).

remove_consumer(Channel_pid, Consumer_code) ->
    gen_server:call(Channel_pid, {remove_consumer, Consumer_code}).

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
%% if a channel with same name is already registered stop this server 
init([Code, Cache_size, Timeout]) ->
    Self = self(),
    case s_global:get_or_register_channel(Code) of
	Self ->
            {ok, #state{code=Code,
			consumer_table=ets:new(consumer_table,[set, public]),
                        cache_size=Cache_size,
                        current_cache_size=0,
                        cache=queue:new(),
                        timeout=Timeout
                       }};
	Pid when is_pid(Pid) -> 
	    {stop, {already_exists, Pid}}
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
handle_call({add_consumer, Consumer_pid, Consumer_code, Handler_type}, _From,
	    #state{consumer_table=Consumer_table,
		   code=Channel_code,
                   timeout=Timeout}=State)->
    %% Send cache to newly subscribed consumer
    gen_server:cast(self(), {send_cache_to_consumer_pid, Consumer_pid}),
    %% Add  Consumer to new channel
    ets:insert(Consumer_table,[{Consumer_code, {Consumer_pid, Handler_type}}]),
    lager:info("Consumer ~p was subscribed to channel ~p with type ~p",[Consumer_code,
									Channel_code,
									Handler_type]),
    %% send subscribtio notification to consumers that subscribed with 'all'' option
    Handler_list = ets:match(Consumer_table, {'$1', {'$2', all}}),

    lists:foldl(fun([C_code, C_pid], Acc) ->
			%% Do not sent message to newly subscribed consumer
			case C_code of
			    Consumer_code -> ok;
			    _ ->s_consumer:push_add_subscribtion(C_pid,
								 Channel_code,
								 Consumer_code),
				Acc
			end
		end, ok, Handler_list),

    Reply = ok,
    
    {reply, Reply, State, Timeout};

%% remove consumer handler consumer and tell consumer
%% to remove table from its own list
handle_call({remove_consumer, Consumer_code}, _From,
	    #state{consumer_table=Consumer_table,
		   code=Channel_code,
                   timeout=Timeout}=State)->

    ets:delete(Consumer_table, Consumer_code),

    Handler_list = ets:match(Consumer_table, {'$1', {'$2', all}}),

    lists:foldl(fun([_C_code, C_pid], _Acc) ->
                        s_consumer:push_remove_subscribtion(C_pid,
                                                            Channel_code,
                                                            Consumer_code)
		end, ok, Handler_list),
    


    
    Reply = ok,
    {reply, Reply, State, Timeout};


handle_call(get_consumers, _From,
	    #state{consumer_table=Consumer_table,
                   timeout=Timeout}=State)->
    Reply ={ok,
	    ets:foldl(fun({Key,_Value},Acc) -> [Key| Acc] end, [], Consumer_table)},
    {reply, Reply, State, Timeout};
    


handle_call(Request, _From, #state{timeout=Timeout}=State) ->
    Reply = ok,
    lager:warning("Unhandled call data reached. ~p~n", [Request]),
    {reply, Reply, State, Timeout}.

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
handle_cast({publish, Message},
	    #state{code=Channel_code,
		   consumer_table=Consumer_table,
                   cache_size=Cache_size,
                   current_cache_size=Current_cache_size,
                   cache=Cache,
                   timeout=Timeout}=State)->
    %% todo run this on another temprary process
    ets:foldl(fun({_Consumer_Code, {Consumer_pid, _Handler_type}}, Acc) ->
		      s_consumer:push_message(Consumer_pid, Channel_code, Message),
		      Acc
	      end, ok, Consumer_table),
    if
        Cache_size =< Current_cache_size ->
            Cache1 = queue:drop(queue:in(Message,Cache)),
            Current_cache_size1 = Current_cache_size;
        Cache_size > Current_cache_size ->
            Cache1 = queue:in(Message,Cache),
            Current_cache_size1 = Current_cache_size + 1
    end,

    {noreply, State#state{current_cache_size=Current_cache_size1,
                          cache=Cache1}, Timeout};


handle_cast({send_cache_to_consumer_pid, Consumer_pid},
            #state{code=Channel_code,
                   cache=Cache,
                   timeout=Timeout}=State) ->
    lists:foreach(fun(Message)->
                          s_consumer:push_cached_message(Consumer_pid, Channel_code, Message)
                  end, queue:to_list(Cache)),

    {noreply, State, Timeout};

handle_cast(Msg, #state{timeout=Timeout}=State) ->
    lager:warning("Unhandled cast message=~p", [Msg]),
    {noreply, State, Timeout}.

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
                            consumer_table=Consumer_table,
                            timeout=Timeout}=State)->
    lager:info("Channel ~p has timeout", [Code]),
    Consumer_list = ets:match(Consumer_table, {'$1', {'$2', '_'}}),
    case Consumer_list of
	[] ->
	    lager:info("~p - All consumers are dead. Channel is dying.", [Code]),
            {stop, normal , State};
	_ ->
            lager:info("~p - There is still alive consumers. Channel will stay alive",
                       [Code]),
            {noreply, State, Timeout}
	end;

handle_info(Info, #state{code=Code,
                         timeout=Timeout}=State) ->
    lager:warning("Unhandled info message in channel ~p (~p)", [Code, Info]),
    {noreply, State, Timeout}.

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
