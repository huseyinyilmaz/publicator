%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2015 by Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%%-------------------------------------------------------------------
-module(session_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(META, #{}).
-define(DELAY, 500).

-define(PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{consumer_code, all},
           {extra_data, []},
           {channel_code, all},
           {publish, true},
           {subscribe, true},
           {create, true},
           {listen_events, true}]]}).

-define(PERSISTENCE_CONFIG,
        {publicator_inmemmory_persistence_backend, []}).

-define(HOST, "127.0.0.1:8766").
%% -define(HOST, "http://www.talkybee.com:8766/").
-define(OPTS, [%% {connect_timeout, 100000000}
               %% {socket_options, [%{keepalive, true},
               %%                   {active, false}]}

              ]).
-define(TIMEOUT, infinity).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    % start test dependencies
%    ibrowse:start(),
    %% ibrowse:set_max_sessions("127.0.0.1", "8766", 30),
    %% ibrowse:set_max_pipeline_size("127.0.0.1", "8766", 30),
    pc_utils:set_env(publicator_core, permission_backend, ?PERMISSION_CONFIG),
    pc_utils:set_env(publicator_core, persistence_backend, ?PERSISTENCE_CONFIG),
    ok = publicator:start(),
    Config.


%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
%    ibrowse:stop(),
    publicator:stop(),
    publicator_core:stop(),
    lager:stop(),
    %% server:stop(),
    %% http:stop(),
    ok.


%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [get_session_http_get,
     get_session_http_post].

get_session_http_get(_Config) ->
    Url = make_url("/session/http/"),
    {ok, "200" , _Headers, Body} =
        ibrowse:send_req(Url, [], get,[],?OPTS, ?TIMEOUT),
    #{<<"type">> := <<"session_created">>, <<"data">> := _Code}
        = jiffy:decode(Body,[return_maps]),
    ok.

get_session_http_post(_Config) ->
    Url = make_url("/session/http/"),
    {ok, "200" , _Headers, Body} =
        ibrowse:send_req(Url, [], post,[],?OPTS, ?TIMEOUT),
    #{<<"type">> := <<"session_created">>, <<"data">> := _Code}
        = jiffy:decode(Body,[return_maps]),
    ok.


make_url(Uri) ->
    "http://" ++ ?HOST ++ Uri.
