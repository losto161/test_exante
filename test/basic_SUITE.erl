%%%-------------------------------------------------------------------
%%% @author losto
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(basic_SUITE).
-author("losto").
-compile(export_all).
%% API
-export([]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(METRIC_NAME, <<"metric1">>).
-define(VALUES, [0.1,0.2,0.3,0.4,0.5]).
-define(RESULT, 0.24285714285714285).
all() ->
    [
        check_api
    ].

init_per_suite(InitConfig) ->
    application:start(metric),
    InitConfig.

end_per_suite(Config) ->
    application:stop(metric),
    Config.

check_api(_Config) ->
    {ok, Res1} = metric_app:report(?METRIC_NAME, 0.1),
    ?assertEqual(true, is_pid(Res1)),
    Res2 = metric_app:report(?METRIC_NAME, 0.1),
    ?assertEqual(ok, Res2),
    lists:foreach(fun(Value) -> metric_app:report(?METRIC_NAME, Value) end, ?VALUES),
    Res3 = metric_app:average(?METRIC_NAME),
    ?assertEqual(0.0, Res3),
    timer:sleep(60000),
    Res4 = metric_app:average(?METRIC_NAME),
    ?assertEqual(?RESULT, Res4),
    timer:sleep(60000),
    Res5 = metric_app:average(?METRIC_NAME),
    ?assertEqual(0.0, Res5).

