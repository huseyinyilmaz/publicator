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
	 get_messages/2, get_messages/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code :: binary(),
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

-spec get(binary()) -> {ok,undefined}.
get(Code) ->
    error_logger:info_report({s_consumer__get, {code, Code}}),
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


-spec get_code(pid()) -> {ok,integer()}.
get_code(Pid) ->
    gen_server:call(Pid,get_code).

get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

-spec get_messages(integer(), integer()) -> {undefined}.
get_messages(Pid, Channel_code) ->
    gen_server:call(Pid, {get_messages, Channel_code}).


-spec push_message(pid(), binary(), binary())-> ok.
push_message(Pid, Resource, Msg) ->
    gen_server:cast(Pid, {push_message, Resource, Msg}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_count() -> {ok, integer()}.
get_count() ->
    {ok, ets:info(consumer, size)}.


				  
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
handle_call({get_messages, Resource_name}, _From, #state{messages=Messages_dict}=State) ->
    Messages = case dict:find(Resource_name, Messages_dict) of
		   error -> [];
		   {ok, Result} -> Result
	       end,
    Messages_dict2 = dict:erase(Resource_name, Messages_dict),
    Reply = {ok, Messages},
    {reply, Reply, State#state{messages=Messages_dict2}};

%% Gets all messages for this user and returns them
handle_call(get_messages, _From, #state{messages=Messages_dict}=State) ->
    Messages = lists:map(fun({_Key, Value}) -> Value end,
			 dict:to_list(Messages_dict)),
    error_logger:info_report({get_messages_callback_all_channels, Messages}),
    Reply = {ok, Messages},
    {reply, Reply, State#state{messages=dict:new()}}.


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

handle_cast({push_message, Channel, Message},
	    #state{messages=Messages_dict}=State) ->
    error_logger:info_report({message_received, Channel, Message}),
    {noreply, State#state{messages=dict:append(Channel, Message, Messages_dict)}};
	     
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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends consumer data to handler
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
send_msg_notification(Handler)->
    Handler ! {have_message, self()}.
