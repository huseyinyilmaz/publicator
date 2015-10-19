%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 12 Dec 2013 by Huseyin Yilmaz <huseyin@ytu-huseyin-yilmaz.local>
%%%-------------------------------------------------------------------
-module(h_consumer).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_code/1]).
-export([subscribe/2]).
-export([subscribe_list/2]).
-export([unsubscribe/2]).
-export([publish/3]).
-export([get_messages/1]).
%% -export([publish/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code :: binary(),
                messages :: list()}).

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
start_link() ->
    gen_server:start_link(?MODULE, [], []).

get_code(Pid) ->
    gen_server:call(Pid, get_code).

subscribe(Pid, Channel_code)->
    gen_server:cast(Pid, {subscribe, Channel_code}).

subscribe_list(Pid, Channel_code_list)->
    gen_server:cast(Pid, {subscribe_list, Channel_code_list}).
    
unsubscribe(Pid, Channel_code)->
    gen_server:cast(Pid, {unsubscribe, Channel_code}).

publish(Pid, Channel_code, Message)->
    gen_server:cast(Pid, {publish, Channel_code, Message}).

get_messages(Pid)->
    gen_server:call(Pid, get_messages).
    
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
init([]) ->
    Code = h_rest_client:get_session(),
    {ok, #state{code=Code,
                messages=[]}}.

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
handle_call(get_code,_From,#state{code=Code}=State) ->
    Result = {ok, Code},
    {reply, Result, State};

handle_call(get_messages, _From, #state{code=Code}=State) ->
    Result = h_rest_client:get_messages(Code),
    {reply, Result, State};
    

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
handle_cast({subscribe, Channel_code}, #state{code=Code}=State) ->
    h_rest_client:subscribe(Code,Channel_code),
    {noreply, State};

handle_cast({subscribe_list, []}, State) ->
    {noreply, State};

handle_cast({subscribe_list, [Channel_code|Rest_list]}, #state{code=Code}=State) ->
    h_rest_client:subscribe(Code, Channel_code),
    handle_cast({subscribe_list, Rest_list}, State);

handle_cast({unsubscribe, Channel_code}, #state{code=Code}=State) ->
    h_rest_client:unsubscribe(Code,Channel_code),
    {noreply, State};
handle_cast({publish, Channel_code, Message}, #state{code=Code}=State) ->
    io:format("-------------------------"),
    io:format("========================="),
    io:format("~p,~p,~p", [Code, Channel_code, Message]),
    h_rest_client:publish(Code, Channel_code, Message),
    {noreply, State};
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
