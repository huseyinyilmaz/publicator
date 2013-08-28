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

-export([get/1, publish/2]).
%% API
-export([start_link/0, add_handler/2, delete_handler/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {consumer :: pid(),
		channel :: binary()}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
get(Name)->
    case s_manager:get(Name) of
	{error, not_found} -> {ok, Resource} = s_resource_sup:start_child(Name),
			      {ok, Resource};
	{ok, Pid} -> {ok, Pid}
    end.

publish(Channel_pid, Message) ->
    error_logger:info_report({s_channel__publish, Channel_pid, Message}),
    ok = gen_event:notify(Channel_pid, Message).

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link().

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
-spec add_handler(pid(), pid()) -> ok | {'EXIT', term()} | term().
add_handler(Pid, Consumer_pid) ->
%% =INFO REPORT==== 14-Aug-2013::23:17:19 ===
%% {s_channel__add_channel,<<"val">>,<0.933.0>,
%%     {'EXIT',
%%         {undef,
%%             [{s_channel,init,
%%                  [[<<"val">>,<0.933.0>],{s_channel,<<"val">>,<0.933.0>}],
%%                  []},
%%              {gen_event,server_add_handler,4,
%%                  [{file,"gen_event.erl"},{line,418}]},
%%              {gen_event,handle_msg,5,[{file,"gen_event.erl"},{line,280}]},
%%              {proc_lib,init_p_do_apply,3,
%%                  [{file,"proc_lib.erl"},{line,239}]}]}}}
			      
    Res = gen_event:add_handler(Pid,
				{?MODULE, Consumer_pid},
				[Consumer_pid]),
    error_logger:info_report({s_channel__add_channel, Consumer_pid, Res}),
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
init([Pid]) ->
    {ok, #state{consumer=Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
-spec handle_event(binary(), term()) -> {ok, term()} |
					{swap_handler, term(), term(), term(), term()} |
					remove_handler.

handle_event(Message, #state{channel=Channel,
			     consumer=Consumer_pid}=State) ->
    error_logger:info_report(aaaaaaaaaaaaaaaaaaaaaaaaaa_handle_event,Message,Consumer_pid),
    %% if user is dead remove handler
    s_user:add_message(Consumer_pid, Channel, Message),
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
    io:format("~p, ~p~n", [_Reason, _State]),
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
