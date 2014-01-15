%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Chat store module provieds api for handling persistence for chat app
%%% @end
%%% Created :  6 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(s_global).

%% API
-export([init/0]).
-export([get_channel/1, set_channel/2]).


-record(code_to_pid, {code :: binary(),
                      pid :: pid()}).

-record(counter, {code :: term(),
                  pid :: number()}).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% initialize mnesia tables for given type
%% Type can be production of development symbol
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    % ensure that /tmp/mnesia directory is present
    ok = filelib:ensure_dir("/tmp/mnesia/ensure.txt"),
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = init_tables().


set_channel(Channel_code, Channel_pid) ->
    Channel = #code_to_pid{code=Channel_code,
                           pid=Channel_pid},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Channel) end),
    {ok, Channel_code}.


get_channel(Channel_code) ->
    Match = #code_to_pid{code=Channel_code, pid='$1'},
    %% Guards = [{'<', Last_msg_code, '$1'}],
    Guards = [],
    Result = ['$_'],
    mnesia:dirty_select(message,[{Match, Guards,Result}]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
table_specs() ->
    [
     [s_channels,
      [{type, bag},
       {ram_copies, [node()| nodes()]},
       {record, code_to_pid},
       {attributes, record_info(fields, code_to_pid)}
      ]],
     [s_consumers,
      [{type, bag},
       {ram_copies, [node()| nodes()]},
       {record, code_to_pid},
       {attributes, record_info(fields, code_to_pid)}
      ]],
     [s_counter,
      [{type, bag},
       {ram_copies, [node()| nodes()]},
       {record, counter},
       {attributes, record_info(fields, counter)}]]
     ].

-spec init_tables() -> ok.
init_tables() ->
    lists:map(fun(Spec)->
                      {atomic, ok} = apply(mnesia, create_table, Spec)
              end, table_specs()),
    ok.
