%%%-------------------------------------------------------------------
%%% @author losto
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(metric_controller).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([
         report/2,
         average/1
        ]).

-define(SERVER, ?MODULE).
-define(BIN2ATOM(Bin), try erlang:binary_to_existing_atom(Bin)
                       catch
                           _:_ -> erlang:binary_to_atom(Bin)
                       end).
-record(metric_controller_state, {period, accumulator, avg}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(report(MetricName::binary(), Value::float()) -> any()).
report(MetricName, Value) ->
    case metric_register:send({?MODULE, MetricName}, {new_value, Value}) of
        {badarg, {_Name, _Msg}} ->
            Spec = #{
                id => {?MODULE, MetricName},
                start => {?MODULE, start_link, [MetricName, Value]}
            },
            supervisor:start_child(metric_mc_sup, Spec);
        Pid when is_pid(Pid) -> ok
    end.
%    ProcName = ?BIN2ATOM(MetricName),
%    case whereis(ProcName) of
%        undefined ->
%            Spec = #{
%                     id => {?MODULE, MetricName},
%                     start => {?MODULE, start_link, [ProcName, Value]}
%                    },
%            supervisor:start_child(metric_mc_sup, Spec);
%        Pid when is_pid(Pid) ->
%            gen_server:cast(ProcName, {new_value, Value})
%    end.

-spec(average(MetricName::binary()) -> Result::float()).
average(MetricName) ->
    gen_server:call({via, metric_register, {?MODULE, MetricName}}, average ).
%%    gen_server:call(?BIN2ATOM(MetricName), average).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Name, Value) ->
    gen_server:start_link({via, metric_register, {?MODULE, Name}}, ?MODULE, [Value], []).
%%    gen_server:start_link({local, Name}, ?MODULE, [Value], []).

init([Value]) ->
    Period = application:get_env(metric, avg_period, 60),
    PeriodMs = Period * 1000,
    State = #metric_controller_state{
               period = PeriodMs,
               accumulator = {Value,0,1},
               avg = 0.0
              },
    erlang:start_timer(PeriodMs, self(), calculate),
    {ok, State}.

handle_call(average, _From, State = #metric_controller_state{avg = Avg}) ->
    {reply, Avg, State};
handle_call(_Request, _From, State = #metric_controller_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #metric_controller_state{}) ->
    {noreply, State}.

handle_info({new_value, Value}, State) ->
    NewState = refresh(Value, State),
    {noreply, NewState};
handle_info({timeout, _Ref, calculate}, State = #metric_controller_state{period = Period, accumulator = Acc}) ->
    Avg = do_avg(Acc),
    erlang:start_timer(Period, self(), calculate),
    {noreply, State#metric_controller_state{accumulator = {0,0,0}, avg = Avg}};
handle_info(_Info, State = #metric_controller_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #metric_controller_state{}) ->
    ok.

code_change(_OldVsn, State = #metric_controller_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

refresh(Value, State = #metric_controller_state{accumulator = Acc}) ->
    NewAcc = do_sum(Acc, Value),
    State#metric_controller_state{accumulator = NewAcc}.

do_avg({0,0,0}) -> 0.0;
do_avg({Sum, Delta, Count}) ->
    (Sum + Delta) / Count.

do_sum({Sum, Delta, Count}, Value) ->
    S = Sum + Value,
    Vs = S - Sum,
    Ss = S - Vs,
    D = (Value - Vs) + (Sum - Ss),
    {S, Delta + D, Count + 1}.
