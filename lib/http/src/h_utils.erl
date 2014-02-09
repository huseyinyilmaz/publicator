%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% utility functions for http application
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(h_utils).

%% API
-export([make_response/2, make_response/3]).
-export([error_response/1, error_response/2]).
-export([no_session_response/0]).
-export([no_session_arg_response/0]).
-export([permission_denied_response/0]).
-export([ok_response/0]).
-export([wrap_with_callback_fun/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec make_response(Type::binary(), Data::term()) -> binary().
make_response(Type, Data) ->
    make_response(Type,Data,[]).

-spec make_response(Type::binary(), Data::term(), Extra_list::list()) -> binary().
make_response(Type, Data, Extra_list) ->
    jiffy:encode({[{<<"type">>, Type},
		   {<<"data">>, Data}| Extra_list]}).

-spec error_response(term()) -> binary().
error_response(Data) ->
    error_response(Data,[]).

-spec error_response(term(),list()) -> binary().
error_response(Data,Extra_list)->
    make_response(<<"error">>,Data,Extra_list).

-spec no_session_response() -> binary().
no_session_response()-> error_response(<<"consumer_not_found">>).

-spec permission_denied_response() -> binary().
permission_denied_response()-> error_response(<<"permission_denied">>).

-spec no_session_arg_response() -> binary().
no_session_arg_response() -> h_utils:error_response(<<"There is no session provided">>).

-spec ok_response() -> binary().
ok_response()->
    make_response(<<"response">>,true).

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

