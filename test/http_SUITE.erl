%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%% Common tests for http handler
%%% @end
%%% Created : 21 Oct 2015 by Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%%-------------------------------------------------------------------
-module(http_SUITE).

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
    publicator:stop(),
    ok.

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [subscribe_get_test,
     subscribe_post_test].

subscribe_get_test(_Config) ->
    Msg = jiffy:encode(#{<<"type">> => <<"subscribe">>,
                         <<"channel_code">> => <<"subscribe_get_test">>}),
    Url = make_url("/" ++ binary:bin_to_list(get_session()) ++ "/http/" ++
                   "?c=" ++ binary:bin_to_list(Msg)),
    {ok, "200" , _Headers, Body} =
        ibrowse:send_req(Url, [], get,[],?OPTS, ?TIMEOUT),
    true = ctcheck:equal(
      #{<<"type">> => <<"subscribed">>, <<"data">> => <<"subscribe_get_test">>},
      jiffy:decode(Body,[return_maps])),
    ok.

subscribe_post_test(_Config) ->
    Request_body = jiffy:encode(#{<<"type">> => <<"subscribe">>,
                         <<"channel_code">> => <<"subscribe_post_test">>}),
    Url = make_url("/" ++ binary:bin_to_list(get_session()) ++ "/http/"),
    {ok, "200" , _Headers, Body} =
        ibrowse:send_req(Url, [], post, Request_body, ?OPTS, ?TIMEOUT),
    true = ctcheck:equal(
             jiffy:decode(Body,[return_maps]),
             #{<<"type">> => <<"subscribed">>, <<"data">> => <<"subscribe_post_test">>}),
    ok.

publish_test_get(_Config) ->
    Session = get_session(),
    ok = subscribe(Session, <<"publish_test_get">>),
    Url = make_url("/" ++ binary:bin_to_list(Session) ++ "/http/" ++
                   "?c={\"type\":\"publish\",\"channel_code\":\"publish_get_test\", \"data\":\"test_data\"}"),
    {ok, "200" , _Headers, Body} =
        ibrowse:send_req(Url, [], get,[],?OPTS, ?TIMEOUT),
    true = ctcheck:equal(
      #{<<"type">> => <<"subscribed">>, <<"data">> => <<"subscribe_get_test">>},
      jiffy:decode(Body,[return_maps])),
    ok.


subscribe(Session, Channel) ->
    Url = make_url("/" ++ binary:bin_to_list(Session) ++ "/http/"),
    Request_body =  jiffy:encode(#{<<"type">> => <<"subscribe">>,
                                   <<"channel_code">> => Channel}),
    {ok, "200" , _Headers, Body} =
        ibrowse:send_req(Url, [], post, Request_body, ?OPTS, ?TIMEOUT),

    true = ctcheck:equal(
             jiffy:decode(Body,[return_maps]),
             #{<<"type">> => <<"subscribed">>, <<"data">> => Channel}),
    ok.


get_session() ->
    Url = make_url("/session/"),
    {ok, "200" , _Headers, Body} =
        ibrowse:send_req(Url, [], get,[],?OPTS, ?TIMEOUT),
    #{<<"type">> := <<"session_created">>, <<"data">> := Code}
        = jiffy:decode(Body,[return_maps]),
    Code.

make_url(Uri) ->
    "http://" ++ ?HOST ++ Uri.
