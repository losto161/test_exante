%%%-------------------------------------------------------------------
%% @doc metric public API
%% @end
%%%-------------------------------------------------------------------

-module(metric_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([
         report/2,
         average/1,
         set_env/1
        ]).

start(_StartType, _StartArgs) ->
    metric_sup:start_link().

stop(_State) ->
    ok.

-spec(report(MetricName::binary(), Value::float()) -> any()).
report(Name, Value) ->
    metric_controller:report(Name, Value).

-spec(average(MetricName::binary()) -> Result::float()).
average(Name) ->
    metric_controller:average(Name).

-spec(set_env(Opts::[{Option::precision | avg_period, Value::integer()}]) -> any()).
set_env(Opts) when is_list(Opts) ->
    lists:foreach(fun
                      ({avg_period, Period}) when is_integer(Period) -> %% in seconds, by default 60
                         application:set_env(metric, avg_period, Period);
                      (_) -> ignore
                 end, Opts);
set_env(_Opts) -> ignore.


%% internal functions
