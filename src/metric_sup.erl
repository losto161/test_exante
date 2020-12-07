%%%-------------------------------------------------------------------
%% @doc metric top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(metric_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    McSup = #{
              id => mc_sup,
              start => {metric_mc_sup, start_link, []},
              type => supervisor
             },
    RegSpec = #{
        id => metric_register,
        start => {metric_register, start_link, []}
    },
    ChildSpecs = [RegSpec, McSup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
