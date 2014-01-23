%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Behivour that implements authentication backend behivour.
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------


-module(s_persistence_backend).

-behaviour(gen_server).
-include("../include/server.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {module::atom(),
                inner_state::term()}).


-callback init(Args :: list(term())) -> 'ok'|tuple('error', Reason :: string()).

-callback handle(Event :: atom()) -> NextEvent :: atom().

-callback sync(Node :: node(), Timeout :: non_neg_integer()) -> 'ok'|tuple('error', Reason :: string()).

-callback insertMessage(Channel :: binary(), Msg:: binary())-> ok.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() ->
%% @end
%%--------------------------------------------------------------------
start_link(Module, Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Module|Args], []).

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
init([Module| Args]) ->
    build_response(Module, Module:init([Args])).
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
handle_call({authenticate, Consumer_code, Auth_info, Extra_data}, _From,
            #state{module=Module, inner_state=Inner_state})
  when is_binary(Consumer_code), is_binary(Auth_info) ->
    Reply = Module:handle_authenticate(Consumer_code, Auth_info, Extra_data, Inner_state),
    {reply, Reply};

handle_call({permission, Consumer_code, Room_code, Extra_data}, _From,
            #state{module=Module, inner_state=Inner_state})
  when is_binary(Consumer_code), is_binary(Room_code) ->
    Reply = Module:handle_permission(Consumer_code, Room_code, Extra_data, Inner_state),
    {reply, Reply};

handle_call(Request, _From, State) ->
    lager:warning("Illegal call request to s_auth_backend Request=~p, State=~p~n",
                  [Request, State]),
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
build_response(Module, Response) ->
%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%
    case Response of
        {ok, State} ->
            {ok, #state{module=Module, inner_state=State}};
        {ok, State, Timeout} ->
            {ok, #state{module=Module, inner_state=State}, Timeout};
        {reply, Reply, State} ->
            {reply, Reply, #state{module=Module, inner_state=State}};
        {reply, Reply, State, Timeout} ->
            {reply, Reply, #state{module=Module, inner_state=State}, Timeout};
        {noreply, State}->
            {noreply, #state{module=Module, inner_state=State}};
        {noreply, State, Timeout}->
            {noreply, #state{module=Module, inner_state=State}, Timeout};
        ignore ->
            ignore;
        {stop, Reason} ->
            {stop, Reason}
    end.
