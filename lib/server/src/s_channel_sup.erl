%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 07 Jun 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(s_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new user
%% @end
%%--------------------------------------------------------------------
start_child(Channel_code) ->
    {Cache_size, Timeout} = s_utils:get_channel_config(Channel_code),
    Args_to_append = [Channel_code, Cache_size, Timeout],
    lager:info("Starting a channel with code ~p , cache size ~p and timeout ~p~n",
               [Channel_code, Cache_size, Timeout]),
    case supervisor:start_child(?SERVER, Args_to_append) of
        {ok, Pid} -> {ok, Pid};
        {error ,{already_exists, Pid}} -> {ok, Pid}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    User = {s_channel, {s_channel, start_link, []},
	    Restart, Shutdown, Type, [s_channel]},

    {ok, {SupFlags, [User]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
