%% @private
-module(syslog_pipeline_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
  %% Initialize the metrics
  syslog_pipeline_metrics:init(),

  {ok, Pools} = application:get_env(syslog_pipeline, workers),
  PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
      PoolArgs = [{name, {local, Name}},
                  {worker_module, Name}] ++ SizeArgs,
      poolboy:child_spec(Name, PoolArgs, WorkerArgs)
  end, Pools),
  {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
