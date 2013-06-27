-module(syslog_pipeline_worker_sup).
-behaviour(supervisor).

%% API.
-export([start_link/4]).

%% supervisor.
-export([init/1]).

%% API.
-export([get_worker/1]).

-spec start_link(syslog_pipeline:ref(), pos_integer(), module(), syslog_pipeline:expander_list())
  -> {ok, pid()}.
start_link(Ref, NumWorkers, BodyParser, Emitters) ->
  syslog_pipeline_server:set_body_parser(Ref, BodyParser),
  syslog_pipeline_server:set_emitters(Ref, Emitters),

  {ok, SupPid} = supervisor:start_link(?MODULE, {
    Ref, NumWorkers
  }),

  WorkerPids = [Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)],

  %% TODO what do we do when a worker crashes? How do we update this list?
  syslog_pipeline_server:set_workers(Ref, WorkerPids),

  {ok, SupPid}.

%% supervisor.

init({Ref, NumWorkers}) ->
  Ref = ets:new(Ref, [
    {write_concurrency, true}, ordered_set, public, named_table]),

  ets:insert(Ref, {worker_index, -1}),

  Procs = [
    {{worker, self(), N}, {syslog_pipeline_worker, start_link, [
      Ref
    ]}, permanent, brutal_kill, worker, []} || N <- lists:seq(1, NumWorkers)
  ],

  {ok, {{one_for_one, 10, 10}, Procs}}.

get_worker(Ref) ->
  Workers = syslog_pipeline_server:get_workers(Ref),
  Count = ets:update_counter(Ref, worker_index, 1),
  Index = Count rem length(Workers),
  lists:nth(Index+1, Workers).
