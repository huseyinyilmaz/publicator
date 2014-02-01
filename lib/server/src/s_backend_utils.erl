%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Utility module for auth and permission backends.
%%% @end
%%% Created :  1 Feb 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(s_backend_utils).

%% API
-export([is_extra_data_passes/2,
        is_equal_or_all/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% check if all filter_extra_data exists in Extra_data.
%% @end
%%--------------------------------------------------------------------

is_extra_data_passes(Extra_data, Filter_extra_data) ->
    is_extra_data_passes(Extra_data, Filter_extra_data, true).

is_extra_data_passes(_Extra_data, _Filter_extra_data, false) -> false;
is_extra_data_passes(_Extra_data, [], true) -> true;
is_extra_data_passes(Extra_data, [{Key, Value}| Filter_extra_data], true)->
    is_extra_data_passes(Extra_data,
                         Filter_extra_data,
                         proplists:get_value(Key, Extra_data, all) == Value).




%%--------------------------------------------------------------------
%% @doc
%% if Filter value is all or Value equals to filter value return true
%% @end
%%--------------------------------------------------------------------

is_equal_or_all(_Value, all) -> true;
is_equal_or_all(Value, Filter_value)-> Value ==Filter_value.

%%%===================================================================
%%% Internal functions
%%%===================================================================
