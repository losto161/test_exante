%%%-------------------------------------------------------------------
%%% @author losto
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(metric_benchmark).
-author("losto").

%% API
-export([
    start_test/3,
    start_sender/3
]).

-spec(start_test(Node::atom(), Limit::integer(), UpdatePeriod::integer()|random) -> ok | {error, cant_connect}).
start_test(Node, Limit, UpdatePeriod) ->
    case net_kernel:connect_node(Node) of
        true -> start_senders(Node, Limit, UpdatePeriod, 0);
        _ -> {error, cant_connect}
    end.

start_senders(_Node, Limit, _UpdatePeriod, Limit) -> ok;
start_senders(Node, Limit, UpdatePeriod, Started) ->
    MetricName = iolist_to_binary([ integer_to_list(X, 16) || <<X>> <= crypto:strong_rand_bytes(16) ]),
    spawn(?MODULE, start_sender, [Node, MetricName, UpdatePeriod]),
    start_senders(Node, Limit, UpdatePeriod, Started + 1).

start_sender(Node, MetricName, random) ->
    start_sender(Node, MetricName, rand:uniform(60));
start_sender(Node, MetricName, UpdatePeriod) when is_integer(UpdatePeriod) ->
    rpc:cast(Node, metric_app, report, [MetricName, rand:uniform()]),
    timer:sleep(UpdatePeriod * 1000),
    start_sender(Node, MetricName, UpdatePeriod).

