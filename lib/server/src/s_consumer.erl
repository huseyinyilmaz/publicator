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
-export([start_link/2, get_user/1, get_code/1,
	 get_count/0, stop/1, add_message/3,
	 get_messages/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code :: binary(),
		messages :: dict(),
		channels :: list()}).

%% -include("c_room_event.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a new user server
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary(),list()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code, Channel_list) ->
    error_logger:info_report({user_start_link, Code, Channel_list}),
    gen_server:start_link(?SERVER, [Code], [Channel_list]).

-spec get_user(binary()) -> {ok,undefined}.
get_user(Code) ->
    %% check if ets table has given Pid
    %% if it doesn't or value is a dead process
    %% set value to undefined.
    case ets:lookup(users, Code) of
	[{Code, Pid}] ->
	    %% if process is dead remove it and
	    %% return not found
	    case is_process_alive(Pid) of
		true -> {ok, Pid};
		false -> ets:delete(users, Code),
			 {error, not_found}
	    end;
	    [] -> {error, not_found}
	end.


-spec get_code(pid()) -> {ok,integer()}.
get_code(Pid) ->
    gen_server:call(Pid,get_code).

-spec get_messages(integer(), integer()) -> {undefined}.
get_messages(Pid, Resource_name) ->
    gen_server:call(Pid, {get_messages, Resource_name}).

-spec add_message(pid(), binary(), binary())-> ok.
add_message(Pid, Resource, Msg) ->
    gen_server:cast(Pid, {add_message, Resource, Msg}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_count() -> {ok, integer()}.
get_count() ->
    {ok, ets:info(user, size)}.


				  
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
init([Code, Channel_list]) ->
    case get_user(Code) of
	{ok , _} -> {stop, already_exists};
	{error, not_found} ->
	    ets:insert(users, {Code, self()}),
	    {ok, #state{code=Code,
			messages=dict:new(),
			channels=Channel_list}}
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

handle_call({get_messages, Resource_name}, _From, #state{messages=Messages_dict}=State) ->
    Messages = dict:fetch(Resource_name, Messages_dict),
    Messages_dict2 = dict:erease(Resource_name, Messages_dict),
    Reply = {ok, Messages},
    {reply, Reply, State#state{messages=Messages_dict2}}.


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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds message to current user
%%
%% @end
%%--------------------------------------------------------------------
%% user_handshake
%% user_removed
%%

handle_cast({receive_message, Resource, Message},
	    #state{messages=Messages_dict}=State) ->
    error_logger:info_report({message_received, Message}),
    {noreply, State#state{messages=dict:append(Resource, Message, Messages_dict)}};
	     
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
%% Sends user data to handler
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
send_msg_notification(Handler)->
    Handler ! {have_message, self()}.
