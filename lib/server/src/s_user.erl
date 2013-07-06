%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(s_user).

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, get_user/1, get_code/1,
	 get_count/0, stop/1, receive_message/2,
	 pop_messages/1, introduce_user/2,
	 exchange_info/2, get_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code, messages}).

%% -include("c_room_event.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts a new user server
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code) ->
    error_logger:info_report({user_start_link, Code}),
    gen_server:start_link(?SERVER, [Code], []).

get_user(Code) ->
    %% check if ets table has given Pid
    %% if it doesn't or value is a dead process
    %% set value to undefined.
    Value =
	case ets:lookup(users, Code) of
	    [{Code, User_pid}] ->
		%% if process is dead remove it and
		%% return not found
		case is_process_alive(User_pid) of
		    true -> User_pid;
		    false -> ets:delete(users, Code),
			     undefined
		end;
	    []           -> undefined
	end,
    %%XXX get user cannot create new user
    case Value of
	undefined ->
	    case s_user_sup:start_child(Code) of
		{ok, Pid} ->
		    {ok, Pid};
		Error -> Error
	    end;
	_ ->
	    % if user is in ets just return it
	    {ok, Value}
    end.


-spec get_code(pid()) -> {ok,integer()}.
get_code(Pid) ->
    gen_server:call(Pid,get_code).

-spec pop_messages(pid()) -> {ok, list()}.
pop_messages(Pid) ->
    gen_server:call(Pid, pop_messages).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_count() -> {ok, integer()}.
get_count() ->
    {ok, ets:info(user, size)}.


%%--------------------------------------------------------------------
%% @doc
%% User with given Pid receives given message.
%% Message here is not necessarily a chat message. It is any erlang
%% message that this user can get. Like removed_user_notification
%% @end
%%--------------------------------------------------------------------
receive_message(Pid, Msg) ->
    gen_server:cast(Pid, {receive_message, Msg}).

introduce_user(Pid, Upid) ->
    gen_server:cast(Pid, {introduce_user, Upid}).

exchange_info(Pid,{Code, Nick}) ->
    gen_server:call(Pid,{exchange_info, Code, Nick}).

get_message(Pid, Message) ->
    gen_server:cast(Pid, Message).
				  
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
    case get_user(Code) of
	{ok , _} -> {stop, already_exists};
	{error, not_found} ->
	    ets:insert(users, {Code, self()}),
	    {ok, #state{code=Code,
			messages=[]}}
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
    {reply, Reply, State}.

%% handle_call(get_info, _From, #state{code=Code,
%% 				    nick=Nick}=State) ->
%%     Reply = {ok, {Code, Nick}},
%%     {reply, Reply, State};

%% handle_call(pop_messages, _From, #state{messages=Messages}=State) ->
%%     Reply = {ok, Messages},
%%     {reply, Reply, State#state{messages=[]}};

%% handle_call({set_handler, Pid}, _From, #state{handler=Handler}=State) ->
%%    case is_process_alive(Handler) of
%%         true ->
%%             {reply, {error, handler_is_alive}, State};
%%         false ->
%%             {reply, ok , State#state{handler=Pid}}
%%     end;

%% handle_call({set_nick, Nick}, _From, State) ->
%%     {reply, ok , State#state{nick=Nick}};

%% handle_call({exchange_info, User_code, User_nick},
%% 	    _From,
%% 	    #state{code=Code,
%% 		   nick=Nick}=State) ->
%%     receive_message(self(), {user_data, User_code, User_nick}),
%%     {reply, {ok, {Code, Nick}}, State}. 


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
%% Special case of message that initiates user_handshake
%%
%% @end
%%--------------------------------------------------------------------
%% user_handshake
%% user_removed
%% 
%% handle_cast({receive_message, #user_handshake{pid=Pid}}, State)
%%   when Pid =:= self() ->
%%     error_logger:info_report({self_receive_message}),

%%     %% Do not handshake yourself
%%     {noreply, State};

%% handle_cast({receive_message, #user_handshake{pid=Pid}},
%% 	    #state{code=Code, nick=Nick}=State) ->
%%     error_logger:info_report({handshake_start}),
%%     {ok, {User_code, User_nick}} = c_user:exchange_info(Pid,{Code, Nick}),
%%     receive_message(self(), {user_data, User_code, User_nick}),
%%     {noreply, State};

handle_cast({receive_message, Message},
	    #state{messages=Messages}=State) ->
    error_logger:info_report({message_received, Message}),
    %% TODO send msg notification after returning the state
    %% send_msg_notification(Handler),
    {noreply, State#state{messages=[Message|Messages]}};
	     
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
