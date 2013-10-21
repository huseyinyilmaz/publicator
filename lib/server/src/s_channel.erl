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
-export([start_link/1]).
-export([publish/2]).
-export([get_channel/1]).
-export([get_consumers/1]).
-export([add_consumer/3]).
-export([remove_consumer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TIMEOUT, 1000).

-record(state, {code :: binary(),
		consumer_table :: ets:tid()
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Code) ->
    gen_server:start_link(?MODULE, [Code], []).


publish(Channel_pid, Message) ->
    gen_server:cast(Channel_pid, {publish, Message}).


get_channel(Channel_code)->
    Key = make_channel_key(Channel_code),
    case gproc:where({n, l, Key}) of
	undefined -> s_channel_sup:start_child(Channel_code);
	Pid when is_pid(Pid) -> {ok, Pid}
    end.

get_consumers(Channel_pid) ->
    gen_server:call(Channel_pid, get_consumers).

add_consumer(Channel_pid, Consumer_pid, Consumer_code) ->
    gen_server:call(Channel_pid, {add_consumer, Consumer_pid, Consumer_code}).

remove_consumer(Channel_pid, Consumer_pid) ->
    gen_server:call(Channel_pid, {remove_consumer, Consumer_pid}).


    %% Key = make_channel_code(Channel_code).
    %% case gproc:get_or_locate({n, l, Key}) 
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
init([Code]) ->
    Key = make_channel_key(Code),
    Self = self(),
    case gproc:reg_or_locate({n,l,Key}) of
	{Self, undefined} ->
	    {ok, #state{code=Code,
			consumer_table=ets:new(consumer_table,[set, public])}};
	{Pid, undefined} when is_pid(Pid) -> 
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
handle_call({add_consumer, Consumer_pid, Consumer_code}, _From,
	    #state{consumer_table=Consumer_table}=State)->
    ets:insert(Consumer_table,[{Consumer_code, Consumer_pid}]),
    Reply = ok,
    {reply, Reply, State, ?TIMEOUT};

handle_call({remove_consumer, Consumer_code}, _From,
	    #state{consumer_table=Consumer_table}=State)->
    ets:delete(Consumer_table, Consumer_code),
    Reply = ok,
    {reply, Reply, State, ?TIMEOUT};


handle_call(get_consumers, _From,
	    #state{consumer_table=Consumer_table}=State)->
    Reply ={ok,
	    ets:foldl(fun({Key,_Value},Acc) -> [Key| Acc] end, [], Consumer_table)},
    {reply, Reply, State, ?TIMEOUT};
    


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
		   consumer_table=Consumer_table}=State)->
    %% todo run this on another temprary process
    ets:foldl(fun({_Consumer_Code, Consumer_pid}, Acc) ->
		      s_consumer:push_message(Consumer_pid, Channel_code, Message),
		      Acc
	      end, ok, Consumer_table),
    {noreply, State, ?TIMEOUT};

handle_cast(_Msg, State) ->
    {noreply, State}.

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

-spec make_channel_key(binary()) -> server:channel_hash_key().
make_channel_key(Channel_code) -> {channel, Channel_code}.
