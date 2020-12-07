%%%-------------------------------------------------------------------
%%% @author losto
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(metric_register).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([
    register_name/2,
    unregister_name/1,
    whereis_name/1,
    send/2
]).

-define(SERVER, ?MODULE).
-define(PROCTAB, metric_proc_registry).
-record(metric_register_state, {tab}).

%%%===================================================================
%%% API
%%%===================================================================

register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

unregister_name(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

whereis_name(Name) ->
    internal_where(ets:lookup(?PROCTAB, Name)).
%%    gen_server:call(?MODULE, {whereis, Name}).

send(Name, Msg) ->
    internal_send(ets:lookup(?PROCTAB, Name), Name, Msg).
%%    gen_server:call(?MODULE, {send, Name, Msg}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    _Tab = ets:new(?PROCTAB, [named_table]),
    {ok, #metric_register_state{}}.

handle_call({register, Name, Pid}, _From, State) ->
    true = ets:insert(?PROCTAB, {Name, Pid}),
    erlang:monitor(process, Pid),
    {reply, yes, State};
handle_call({unregister, Name}, _From, State) ->
    Result = internal_unregister(?PROCTAB, Name),
    {reply, Result, State};
handle_call({whereis, Name}, _From, State) ->
    Result = internal_where(ets:lookup(?PROCTAB, Name)),
    {reply, Result, State};
handle_call({send, Name, Msg}, _From, State) ->
    Result = internal_send(ets:lookup(?PROCTAB, Name), Name, Msg),
    {reply, Result, State};

handle_call(_Request, _From, State = #metric_register_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #metric_register_state{}) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _}, State) ->
    internal_unregister(?PROCTAB, Pid),
    {noreply, State};

handle_info(_Info, State = #metric_register_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #metric_register_state{}) ->
    ok.

code_change(_OldVsn, State = #metric_register_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

internal_where([]) -> undefined;
internal_where([{_Name, Pid}]) -> Pid.

internal_send([], Name, Msg) -> {badarg, {Name, Msg}};
internal_send([{_Name, Pid}], _Name, Msg) ->
    Pid ! Msg,
    Pid.

internal_unregister(?PROCTAB, Pid) when is_pid(Pid) ->
    true = ets:match_delete(?PROCTAB, {'_', Pid});
internal_unregister(?PROCTAB, Name) ->
    true = ets:delete(?PROCTAB, Name),
    Name.