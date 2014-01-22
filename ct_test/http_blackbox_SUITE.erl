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
    ibrowse:start(),
    %% ok = server:start(),
    %% ok = http:start(),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ibrowse:stop(),
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
    [test_100_room_2_consumer,
     test_10_room_20_consumer,
     test_2_room_100_consumer].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
test_100_room_2_consumer() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
test_2_room_100_consumer() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
test_10_room_20_consumer() -> 
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
create_channel(Channel_count, Consumer_count, _Message_count) ->
    %% Create Channel_ids
    Channel_code_list = lists:map(fun(Num)->
					  Bin = list_to_binary(integer_to_list(Num)),
					  << <<"channel_">>/binary,
					     Bin/binary >>
				  end, lists:seq(1,Channel_count)),

    %% Create_consumer_codes
    Consumer_pid_list = [begin
                             {ok, Consumer_pid} = h_consumer:start_link(),
                             Consumer_pid
                         end
                         || _C <- lists:seq(1, Consumer_count)],

    %% Subscribe to channels
    lists:foreach(
      fun(Pid)->
              ct:pal("Subscribe_list"),
              h_consumer:subscribe_list(Pid, Channel_code_list)
      end,
      Consumer_pid_list),

    [Consumer_pid | _] = Consumer_pid_list,

    lists:foreach(
      fun(Channel_code)->
              h_consumer:publish(Consumer_pid,
                                 Channel_code,
                                 <<"test_msg">>)
      end,
      Channel_code_list
     ),

    Expected_message_list = lists:keysort(1,
                                          [{C, [{[{<<"message">>,<<"test_msg">>}]}]}
                                           ||C <- Channel_code_list]),

    lists:foreach(
      fun(Pid)->
              %% Result_structure =
              %% [{[{<<"channel_1">>,[{[{<<"message">>,<<"test_msg">>}]}]},
              %%    {<<"channel_5">>,[{[{<<"message">>,<<"test_msg">>}]}]},
              %%    {<<"channel_4">>,[{[{<<"message">>,<<"test_msg">>}]}]},
              %%    {<<"channel_3">>,[{[{<<"message">>,<<"test_msg">>}]}]},
              %%    {<<"channel_2">>,[{[{<<"message">>,<<"test_msg">>}]}]}]}]
              Result_text = h_consumer:get_messages(Pid),
              Result_structure = jiffy:decode(Result_text),
              [{Result_list}] = Result_structure,
              Sorted_result_list = lists:keysort(1, Result_list),
              ct:pal("Sorted_result_list=~p~nExpected_message_list=~p~n",
                     [Sorted_result_list, Expected_message_list]),
              Expected_message_list = Sorted_result_list

      end,
      Consumer_pid_list
     ),

    %% Create accepted message_list
    %% Send message from channels
    ct:pal("Create Channel").
    %get subscribtions complete messages
    %% Res = lists:map(fun(Code)->
    %%     		    receive
    %%     			{finished, Code} -> ok
    %%     		    end
    %%     	    end,
    %%     	    Consumer_code_list),
    %% ct:pal("Subscribtions complete,~p~n", [Res]).



test_100_room_2_consumer(_Config) ->
    ct:log("LOG test"),
    ct:print("PRINT test"),
    create_channel(100, 2, 1),
    ok.


test_10_room_20_consumer(_Config) ->
    ct:log("LOG test"),
    ct:print("PRINT test"),
    create_channel(10, 20, 1),
    ok.

test_2_room_100_consumer(_Config) ->
    ct:log("LOG test"),
    ct:print("PRINT test"),
    create_channel(2, 100, 1),
    ok.
