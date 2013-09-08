%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@saturn.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Basic tests for development. until got tests working.
%%% @end
%%% Created : 26 Aug 2013 by Huseyin Yilmaz <huseyin@saturn.local>
%%%-------------------------------------------------------------------
-module(tests).

%% API
-export([start/0, get_session/0, setup_server/0, get_subscribtions/1,
	 subscribe/2, blackbox_test/2, channel_test/3,
	create_consumer/2,test/1, find_square/2]).

%%%===================================================================
%%% API
%%%===================================================================
setup_server()->
    error_logger:info_report("Setup server"),
    %% server:start(),
    %% http:start(),
    %% code:add_patha("deps/jiffy/ebin"),
    %% code:add_patha("../../../deps/jiffy/ebin"),
    inets:start().

cleanup_server()->
    error_logger:info_report("Cleanup server"),
    %% http:stop(),
    %% server:stop(),
    inets:stop().

request(Uri, Method) ->
    Host = "http://localhost:8766/",
    Url = Host ++ Uri,
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	httpc:request(Method, {Url, []}, [], []),
    Body.

request2(Uri) ->
    Host = "http://localhost:8766/",
    Url = Host ++ Uri,
    {ok, {{_Version, 204, _ReasonPhrase}, _Headers, Body}} =
	httpc:request(post, {Url, [], "application/json", []}, [], [{body_format, string}]),
    Body.
    
get_session()->
    Uri = "session/",
    Body = request(Uri, get),
    {[{<<"session">>,Session_id}]} = jiffy:decode(Body),
    Session_id.

get_subscribtions(Session_id) ->
    Uri = binary_to_list(Session_id) ++ "/subscribtions/",
    Body = request(Uri, get),
    jiffy:decode(Body).

subscribe(Session_id, Channel_code) ->
    Uri = binary_to_list(Session_id) ++
	"/subscribtions/" ++
	binary_to_list(Channel_code) ++ "/",
    request2(Uri).    
    

create_channel(Channel_code, Consumer_count)->
    Consumer_list = [get_session() || _C <- lists:seq(1, Consumer_count)],
    error_logger:info_report({create_room,Consumer_list}),
    timer:sleep(3000),
    lists:map(fun(C)->subscribe(C, Channel_code)end,Consumer_list),
    Consumer_list.
    
create_consumer(Consumer_code, Channel_pids)->
    lists:map(fun({Code, Pid})->
		      subscribe(Consumer_code, Code),
		      timer:sleep(2000),
		      Pid ! {consumer_done,
			     Consumer_code,
			     Code}
	      end,
	      Channel_pids).

channel_test(Channel_code, Consumer_code_list, Pid)->
    io:format("XXXXX",[]),
    error_logger:info_report({channel_test__consumer_list, Consumer_code_list}),
    Result = lists:all(
	       fun(C)->
		       receive
			   {consumer_done,
			    C,
			    Channel_code} -> true
		       end
	       end,
	       Consumer_code_list),
    {channel_done,
     Channel_code,
     Result} ! Pid.



blackbox_test(Channel_count, Consumer_count)->
    %% Create Channel_ids
    Channel_code_list = lists:map(fun(Num)->
				     Bin = integer_to_binary(Num),
				     << <<"channel_">>/binary,
					Bin/binary >>
				  end, lists:seq(1,Channel_count)),
    
    Consumer_code_list = [get_session() || _C <- lists:seq(1, Consumer_count)],
    Channel_pids = [{C,
    		     spawn(tests, channel_test, [C, Consumer_code_list,self()])}
					|| C <- Channel_code_list],
    _Consumer_pids = [spawn(tests, create_consumer, [C, Channel_pids]) || C <- Consumer_code_list],

     Result = lists:map(
     		fun(C)->
     			receive
     			    {channel_done,
     			     C,
     			     Result} -> {C,Result}
     			end
     		end,
     		Channel_code_list),
    {result,Result}.






start()->
    setup_server(),
    blackbox_test(100,1),
    cleanup_server().


find_square(Number, Pid)->
    Pid! {result, self(), Number*Number}.

test(Count)->
    Find_square_process_pids = [spawn(tests, find_square,[Num, self()])
				||Num <- lists:seq(1, Count)],
    Result = lists:map(fun(Pid)->
			       receive
				   {result,
				    Pid,
				    Result} -> Result
				   end
		       end,
		       Find_square_process_pids),
    io:format("~w",[Result]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
