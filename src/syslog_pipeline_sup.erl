%% @private
-module(syslog_pipeline_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link(atom(), [{atom(), term()}]) -> {ok, pid()}.
start_link(Name, Env) ->
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [Name, Env]).

%% supervisor.

init([Name, Env]) ->
  Name = ets:new(Name, [{read_concurrency, true}, ordered_set, named_table, public]),

  true = ets:insert(Name, {env, Env}),

  {ok, {{one_for_one, 10, 10}, []}}.
