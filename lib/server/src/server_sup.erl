%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_permanent_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================
start_permanent_child(Mod, Args) when is_atom(Mod),
                                      is_list(Args)->
    Shutdown = 10000,
    lager:debug("**************************************"),
    lager:debug("******** module=~p args=~p~n", [Mod, Args]),
    
    _Val = supervisor:start_child(?SERVER,
                           {Mod,
                            {Mod, start_link, Args},
                            permanent,
                            Shutdown,
                            worker,
                            [s_auth_backend]}),
    lager:debug("Val=~p~n", [_Val]),
    _Val
    .

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,

    Channel_sup = {s_channel_sup, {s_channel_sup, start_link, []},
		    Restart, Shutdown, Type, [c_channel_sup]},

    Consumer_sup = {s_consumer_sup, {s_consumer_sup, start_link, []},
		Restart, Shutdown, Type, [s_consumer_sup]},
    lager:info("End of server_sup init function"),
    {ok, {SupFlags, [Consumer_sup, Channel_sup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
