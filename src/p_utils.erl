%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% utility functions for http application
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(p_utils).

%% API
-export([make_response/2, make_response/3]).
-export([message_response/1]).
-export([message_list_response/1]).
-export([serialize_response/1]).
-export([error_response/1, error_response/2]).
-export([no_session_response/0]).
-export([no_session_arg_response/0]).
-export([permission_denied_response/0]).
-export([invalid_channel_code_response/0]).
-export([ok_response/0]).
-export([wrap_with_callback_fun/2]).
-export([get_request_text/1]).
-export([parse_request_text/1]).
-export([parse_request_text/2]).

-include("../deps/publicator_core/include/publicator_core.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns maps of body of request or querystring of request.
%% @end
%%--------------------------------------------------------------------

get_request_text(Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Raw_data, Req_read} = cowboy_req:body(Req);
        false ->
            Default_value = <<"{\"meta\":{}, \"data\":\"\"}">>,
            % get content from qs
            {Raw_data, Req_read} = cowboy_req:qs_val(<<"c">>, Req, Default_value)
    end,
    {Raw_data, Req_read}.

parse_request_text(Text) -> parse_request_text(Text, undefined).
     
parse_request_text(Text, Producer_code) ->    
    Defaults = #{<<"type">> => undefined,
                 <<"data">> => undefined,
                 <<"meta">> => undefined,
                 <<"channel_code">> => undefined},
    #{<<"type">> := Type,
      <<"data">> := Data,
      <<"meta">> := Meta,
      <<"channel_code">> := Channel_code} = maps:merge(Defaults,
                                                       jiffy:decode(Text, [return_maps])),
    #message{type=Type, data=Data, meta=Meta,
             producer_code=Producer_code, channel_code=Channel_code}.
    
-spec make_response(Type::binary(), Data::any()) -> binary().
make_response(Type, Data) ->
    make_response(Type,Data,[]).

-spec make_response(Type::binary(), Data::any(), Extra_list::list()) -> binary().
make_response(Type, Data, Extra_list) ->
    jiffy:encode({[{<<"type">>, Type},
		   {<<"data">>, Data}| Extra_list]}).

-spec error_response(any()) -> binary().
error_response(Data) ->
    error_response(Data,[]).

-spec error_response(any(),list()) -> binary().
error_response(Data,Extra_list)->
    make_response(<<"error">>,Data,Extra_list).

-spec no_session_response() -> binary().
no_session_response()-> error_response(<<"producer_not_found">>).

-spec permission_denied_response() -> binary().
permission_denied_response()-> error_response(<<"permission_denied">>).

-spec no_session_arg_response() -> binary().
no_session_arg_response() -> error_response(<<"There is no session provided">>).

-spec ok_response() -> binary().
ok_response()->
    make_response(<<"response">>,true).

-spec invalid_channel_code_response() ->binary().
invalid_channel_code_response() ->
    error_response(<<"invalid_channel_code">>).


serialize_response(#message{type= message,
                            channel_code=Channel_code,
                            data=Message,
                            meta=Meta})->
    lager:info("-------------------------------!!"),
    #{<<"type">> => <<"message">>,
      <<"data">> => Message,
      <<"channel_code">> => Channel_code,
      <<"meta">> => Meta};

serialize_response(#message{type=add_subscribtion,
                            channel_code=Channel_code,
                            producer_code=Producer_code})->
    {[{<<"type">>, <<"add_subscribtion">>},
      {<<"data">>, Producer_code},
      {<<"channel_code">>, Channel_code}]};

serialize_response(#message{type=remove_subscribtion,
                            channel_code=Channel_code,
                            producer_code=Producer_code})->
    {[{<<"type">>, <<"remove_subscribtion">>},
      {<<"data">>, Producer_code},
      {<<"channel_code">>, Channel_code}]};

serialize_response(Msg)->
    lager:warning("Unhandled messagetype ~p",[Msg]),
    {[{<<"type">>, <<"unhandled_info">>},
      {<<"data">>, tuple_to_list(Msg)}]}.


message_response(Msg)->
    jiffy:encode(serialize_response(Msg)).

-spec message_list_response(Msg_list::[tuple()])->binary().
message_list_response(Msg_list)->
    jiffy:encode(lists:map(fun serialize_response/1,
                           Msg_list)).
%%--------------------------------------------------------------------
%% @doc
%% If callback is undefined returns body as it is.
%% if callback is a binary returns returns <<"callbackname(body);">>
%% @end
%%--------------------------------------------------------------------
wrap_with_callback_fun(undefined, Body) -> Body;
wrap_with_callback_fun(Callback, Body) ->
    [Callback, <<"(">> , Body, <<");">>].
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

