%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 06 Jun 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(s_consumer_sup).

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
%% Starts a new consumer
%% @end
%%--------------------------------------------------------------------
-spec start_child(Auth_info::binary()) -> {ok, Code::binary(), Pid::pid()}.
start_child(Auth_info) ->
    Code = s_utils:generate_code(),
    lager:info("~p~n", [{start_new_consumer, Code, Auth_info}]),
    Args_to_append = [Code],
    case supervisor:start_child(?SERVER, Args_to_append) of
	{ok, Pid} -> {ok, Code, Pid};
	{error, {already_exists, _Pid}} -> start_child(Auth_info)
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

    Consumer = {consumer, {s_consumer, start_link, []},
	    Restart, Shutdown, Type, [s_consumer]},

    {ok, {SupFlags, [Consumer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
