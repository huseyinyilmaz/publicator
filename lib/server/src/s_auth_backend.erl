%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Behivour that implements authentication backend behivour.
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(s_auth_backend).

-behaviour(gen_server).
-include("../include/server.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {module::atom(),
                consumer_code::binary()|_,
                group::atom(),
                auth_info::binary()|_}).
%%--------------------------------------------------------------------
%% @doc
%% Initializes backend
%% @end
%%--------------------------------------------------------------------
-callback init_auth(Args::list(term())) -> 'ok'|tuple('error', Reason::string()).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback authenticate(Consumer_Code::binary(),
                       Auth_info::binary()) -> not_allowed| ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback get_permissions(Consumer_Code::binary(),
                          Room_code::binary()) -> permission_type().

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
    %% lager:debug("==================="),
    %% lager:debug("Start auth  backend"),
    lager:debug("///////////////////////////"),
    lager:debug("call s_authbackend:start_link/1 "),
    lager:debug("Module=~p, Args=~p~n", [Module, Args]),
    {ok, #state{module=Module,
                consumer_code=proplists:get_value(consumer_code, Args, all),
                group=proplists:get_value(group, Args, all),
                auth_info=proplists:get_value(consumer_code, Args, all)}}.

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
