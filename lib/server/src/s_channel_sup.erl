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
-export([start_link/0, start_child/3]).

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
start_child(Consumer_code, Channel_code, Extra_data) ->
    {Permission_module, Permission_args} = s_permission_backend:get_permission_backend(),
    Permission_state = Permission_module:init_state(Permission_args),
    Perms = Permission_module:get_permissions(Consumer_code,
                                               Channel_code,
                                               Extra_data,
                                              Permission_state),
    case s_permission_backend:has_permission(Perms, can_create_channel) of
        true ->
            lager:info("Start a channel"),
            Args_to_append = [Channel_code],
            case supervisor:start_child(?SERVER, Args_to_append) of
                {ok, Pid} -> {ok, Pid};
                {error ,{already_exists, Pid}} -> {ok, Pid}
            end;
        false ->
            {error, permission_denied}
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
