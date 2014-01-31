%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% This module is wrapper over global registery
%%% @end
%%% Created :  6 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(s_global).

%% API
-export([get_channel/1, set_channel/2]).
-export([get_or_register_channel/1]).
-export([get_consumer/1, set_consumer/2]).
-export([get_or_register_consumer/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a channel pid for given channel code.
%% @end
%%--------------------------------------------------------------------
-spec get_channel(Channel_code::binary()) -> pid()| undefined.
get_channel(Channel_code)->
    get_object(make_channel_key(Channel_code)).

%%--------------------------------------------------------------------
%% @doc
%% Sets given channel pid to given channel code.
%% @end
%%--------------------------------------------------------------------
-spec set_channel(Channel_code::binary(), Channel_pid::pid()) -> pid().
set_channel(Channel_code, Channel_pid)->
    set_object(make_channel_key(Channel_code), Channel_pid).

%%--------------------------------------------------------------------
%% @doc
%% Returns pid that registered to given code.
%% if there is no registered pid, registers current process to
%% given code and returns current pid.
%% @end
%%--------------------------------------------------------------------
-spec get_or_register_channel(Channel_code::binary()) -> pid().
get_or_register_channel(Channel_code)->
    get_or_register_object(make_channel_key(Channel_code)).


%%--------------------------------------------------------------------
%% @doc
%% Returns a consumer pid for given consumer code.
%% @end
%%--------------------------------------------------------------------
-spec get_consumer(Channel_code::binary()) -> pid()| undefined.
get_consumer(Channel_code)->
    get_object(make_consumer_key(Channel_code)).

%%--------------------------------------------------------------------
%% @doc
%% Sets given consumer pid to given consumer code.
%% @end
%%--------------------------------------------------------------------
-spec set_consumer(Channel_code::binary(), Channel_pid::pid()) -> pid().
set_consumer(Channel_code, Channel_pid)->
    set_object(make_consumer_key(Channel_code), Channel_pid).

%%--------------------------------------------------------------------
%% @doc
%% Returns pid that registered to given code.
%% if there is no registered pid, registers current process to
%% given code and returns current pid.
%% @end
%%--------------------------------------------------------------------
-spec get_or_register_consumer(Channel_code::binary()) -> pid().
get_or_register_consumer(Channel_code)->
    get_or_register_object(make_consumer_key(Channel_code)).



%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Creates global registry key for given channel code.
-spec make_channel_key(Channel_code::binary()) -> term() | undefined.
make_channel_key(Channel_code)->
    {channel, Channel_code}.

%% Creates global registry key for given consumer code.
-spec make_consumer_key(Channel_code::binary()) -> term() | undefined.
make_consumer_key(Consumer_code)->
    {consumer, Consumer_code}.

%% Get object from global registery
-spec get_object(Key::term()) -> pid()| undefined.
get_object(Key)->
    global:whereis_name(Key).

%% set given pid to given key in global registery
-spec set_object(Key::term(), Pid::pid()) -> pid().
set_object(Key, Pid)->
    yes = global:register_name(Key, Pid),
    Pid.

%% gets given code from globalregistery
%% if it does not exists, sets current process
%% to given registry and return that current pid
-spec get_or_register_object(Key::term()) -> pid().
get_or_register_object(Key)->
    case get_object(Key) of
        undefined ->
            set_object(Key, self());
        Pid when is_pid(Pid) ->
            Pid
    end.



%%--------------------------------------------------------------------
%% -@doc
%% initialize mnesia tables for given type
%% Type can be production of development symbol
%% @end
%%--------------------------------------------------------------------
%% -spec init() -> ok.
%% init() ->
%%     % ensure that /tmp/mnesia directory is present
%%     ok = filelib:ensure_dir("/tmp/mnesia/ensure.txt"),
%%     stopped = mnesia:stop(),
%%     ok = mnesia:delete_schema([node()]),
%%     ok = mnesia:create_schema([node()]),
%%     ok = mnesia:start(),
%%     ok = init_tables().


%% set_consumer(Channel_code, Channel_pid) ->
%%     Channel = #code_to_pid{code=Channel_code,
%%                            pid=Channel_pid},
%%     {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Channel) end),
%%     {ok, Channel_code}.


%% get_consumer(Channel_code) ->
%%     Match = #code_to_pid{code=Channel_code, pid='$1'},
%%     %% Guards = [{'<', Last_msg_code, '$1'}],
%%     Guards = [],
%%     Result = ['$_'],
%%     mnesia:dirty_select(message,[{Match, Guards,Result}]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% table_specs() ->
%%     [
%%      [s_channels,
%%       [{type, bag},
%%        {ram_copies, [node()| nodes()]},
%%        {record, code_to_pid},
%%        {attributes, record_info(fields, code_to_pid)}
%%       ]],
%%      [s_consumers,
%%       [{type, bag},
%%        {ram_copies, [node()| nodes()]},
%%        {record, code_to_pid},
%%        {attributes, record_info(fields, code_to_pid)}
%%       ]],
%%      [s_counter,
%%       [{type, bag},
%%        {ram_copies, [node()| nodes()]},
%%        {record, counter},
%%        {attributes, record_info(fields, counter)}]]
%%      ].

%% -spec init_tables() -> ok.
%% init_tables() ->
%%     lists:map(fun(Spec)->
%%                       {atomic, ok} = apply(mnesia, create_table, Spec)
%%               end, table_specs()),
%%     ok.
