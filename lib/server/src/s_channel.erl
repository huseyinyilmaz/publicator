%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Handles all room events
%%% @end
%%% Created : 21 Feb 2013 by Huseyin Yilmaz <>
%%%-------------------------------------------------------------------
-module(s_channel).

-behaviour(gen_event).

-export([publish/3]).
%% API
-export([start_link/0, stop/1, add_handler/3, delete_handler/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {consumer :: pid(),
		channel :: binary()}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

publish(Channel_pid, Consumer_pid, Message) ->
    error_logger:info_report({s_channel__publish, Channel_pid, Consumer_pid, Message}),
    ok = gen_event:notify(Channel_pid, {Consumer_pid, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link().


stop(Pid)->
    gen_event:stop(Pid).
%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
-spec add_handler(pid(),binary() ,pid()) -> ok | {'EXIT', term()} | term().
add_handler(Pid, Channel_code, Consumer_pid) ->
    Res = gen_event:add_handler(Pid,
			       {?MODULE, Consumer_pid},
			       [Consumer_pid, Channel_code]),
    error_logger:info_report({s_channel__add_channel,Channel_code, Pid, Consumer_pid}),
    Res.
    
    

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_handler(pid(), pid()) -> ok.
delete_handler(Pid, Upid) ->
    gen_event:delete_handler(Pid, {?MODULE, Upid}, [delete_handler, Upid]).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([Pid, Channel_code]) ->
    {ok, #state{consumer=Pid,
		channel=Channel_code}}.

%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
-spec handle_event(term(), term()) ->
			  {ok,_} |
			  {ok,_,hibernate} |
			  {swap_handler,_,_,atom() | {atom(),_},_}.


%%% if owner of message is this consumer do not send message
handle_event({_Consumer_pid, _Message}, #state{consumer=_Consumer_pid}=State) ->
    error_logger:info_report({owner_handle_event}),
    {ok, State};

handle_event({_Owner_Consumer_pid, Message}, #state{channel=Channel_code,
						    consumer=Consumer_pid}=State) ->
    error_logger:info_report({other_handle_event}),
    %% if user is dead remove handler
    s_consumer:push_message(Consumer_pid, Channel_code, Message),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("s_channel_terminate has been called ~n ~p, ~n~p~n", [_Reason, _State]),
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
