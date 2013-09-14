%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@new-host.home>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Blackbox tests for http interface
%%% @end
%%% Created : 13 Sep 2013 by Huseyin Yilmaz <huseyin@new-host.home>
%%%-------------------------------------------------------------------
-module(http_blackbox_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


-type configuration() :: [{term(), term()}].
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
-spec suite () -> configuration().
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
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
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
%%               void() | {save_config,Config1}
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
    %% ok = server:start(),
    %% ok = http:start(),
    ok = inets:start(),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    %% ok = http:stop(),
    %% ok = server:stop(),
    ok = inets:stop(),
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [test_1_room_1000_consumer].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
test_1_room_1000_consumer() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
create_channel(Channel_count, Consumer_count) ->

    %% Create Channel_ids
    Channel_code_list = lists:map(fun(Num)->
					  Bin = integer_to_binary(Num),
					  << <<"channel_">>/binary,
					     Bin/binary >>
				  end, lists:seq(1,Channel_count)),

    %% Create_consumer_codes
    Consumer_code_list = [h_rest_client:get_session() || _C <- lists:seq(1, Consumer_count)],

    Consumer_pids = [spawn(http_blackbox_SUITE, consumer,
			   [C,
			    [{Consumer_code, Channel_code}
			     || Consumer_code <- Consumer_code_list,
				Channel_code <- Channel_code_list,
				Consumer_code =/= C],
			    Channel_code_list,
			    self()])
		     || C <- Consumer_code_list],
    %get subscribtions complete messages
    Res = lists:map(fun(Code)->
			    receive
				{subscribed, Code} -> ok
			    end
		    end,
		    Consumer_code_list),
    ct:pal("Subscribtions complete,~p~n", [Res]).

    

test_1_room_1000_consumer(_Config) ->
    ct:log("LOG test"),
    ct:print("PRINT test"),
    create_channel(2,2),
    ok.


    
%% Code : Consumer_code
%% Message_expected: Expected_message_list [{Channel_code, Consumer_code}]
%% Send_channel_list: Channel list that will sent messages to
%% message format will be {message, Channel_code, Consumer_code}
%% Parent_pid: Pid of parent that will be send a consumer completed message
%% once expected messages will be send and all expected messages will be received
consumer(Code, Message_expected, Send_channel_list, Parent_pid) ->
    %get channel_list to subscribe
    Channel_list = sets:to_list(
		     sets:from_list(
		       [Channel_code ||
			   {_Consumer_code, Channel_code}<- Message_expected])),
    %send messages
    Val = lists:map(fun(Channel_code)->
			    ct:pal("presubscribe, ~n ~p ~n ~p ~n",[Channel_code, Code]),
			    Res = h_rest_client:subscribe(Code,Channel_code),
			    ct:pal("subscribed, ~n ~p ~n ~p ~n ~p ~n",[Res, Channel_code, Code])
		    end,
		    Channel_list),
    %send subscribtion complete message and wait for continue
    ct:pal("AAAsubscribtions completed ~p~n" ,[Code]),
    Parent_pid ! {subscribed, Code},
    receive
	receive_continue -> ok
    end,
    
    ct:pal("in consumer= ~p ~p ~p ~p ~p~n", [Code, Message_expected, Send_channel_list,Channel_list, Val]),
    
    ok.
    
