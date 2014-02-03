%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(publicator_http_auth_backend).
-behivour(s_auth_backend).
%% API
-export([init_state/1, authenticate/4]).

-include("../include/server.hrl").

-record(state, {url::list()|binary()}).

-define(OPTS, []).
-define(TIMEOUT, 10000).
-define(RETRY, 3).
%%%===================================================================
%%% API
%%%===================================================================
-callback init_state(Auth_args::term()) -> New_state::term().
init_state(Auth_args) ->
    #state{url=proplists:get_value(url, Auth_args)}.



%% -spec init([Args::term()]) -> {ok, State::term()}.
%% init([Args])->
%%     %% lager:debug("==================="),
%%     %% lager:debug("Start auth  backend"),
%%     lager:debug("///////////////////////////"),
%%     lager:debug("call s_authbackend:start_link/1 "),
%%     lager:debug("Args=~p~n", [Args]),
%%     Auth_list = [#auth_filter{consumer_code=proplists:get_value(consumer_code, Arg, all),
%%                               group=proplists:get_value(group, Arg, all),
%%                               auth_info=proplists:get_value(consumer_code, Arg, all)}
%%                  || Arg <- Args],
%%     {ok, #state{filter_list=Auth_list}}.


-spec authenticate(Consumer_code::binary(),
                   Auth_info::binary(),
                   Extra_data::list(),
                   State::term()) -> boolean().
authenticate(Consumer_code, Auth_info, Extra_data, State)->
    authenticate(Consumer_code, Auth_info, Extra_data, State, ?RETRY).

authenticate(_Consumer_code, _Auth_info, _Extra_data, _State, 0) ->
    false;
authenticate(Consumer_code, Auth_info, Extra_data, #state{url=Url}=State, Retry) ->
    Data= jiffy:encode({[{<<"consumer_code">>, Consumer_code},
                         {<<"auth_info">>, Auth_info},
                         {<<"extra_data">>, {Extra_data}}]}),
    case ibrowse:send_req(Url, [{"Content-Type", "text/html"}],
                          post, Data, ?OPTS, ?TIMEOUT) of
        {ok, "200", _Headers, Body} ->
            lager:debug("Data returned from auth server=~p~n", [Body]),
            {[{<<"authenticate">>,Result}]} = jiffy:decode(Body),
            Result;
        {error, req_timedout}->
            lager:warning("Could not reach the auth server. retrying ~p more times", [Retry]),
            authenticate(Consumer_code, Auth_info, Extra_data, State, Retry-1)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
