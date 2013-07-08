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
  syslog_pipeline_server = ets:new(syslog_pipeline_server, [
    {read_concurrency, true}, ordered_set, public, named_table]),

  Procs = [
    {syslog_pipeline_server, {syslog_pipeline_server, start_link, []},
      permanent, 5000, worker, [syslog_pipeline_sup]},
    {pooler_sup, {pooler_sup, start_link, []},
      permanent, infinity, supervisor, [pooler_sup]}
  ],

  {ok, {{one_for_one, 10, 10}, Procs}}.
