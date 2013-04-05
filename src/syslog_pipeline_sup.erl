%% @private
-module(syslog_pipeline_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link(atom(), integer(), integer()) -> {ok, pid()}.
start_link(Name, HandleSize, ServerSize) ->
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [Name, HandleSize, ServerSize]).

%% supervisor.

init([Name, HandleSize, ServerSize]) ->
  %% Initialize the metrics
  syslog_pipeline_metrics:init(),

  Name = ets:new(Name, [{read_concurrency, true}, named_table]),

  RingTable = concat_tuples(Name, '_ring_buf'),

  {ok, RingBuf} = ring_buf:start_link(HandleSize, RingTable),
  {ok, ServerRing} = server_ring:start_link(syslog_pipeline_worker, [], ServerSize, concat_tuples(Name, '_server_ring')),

  % Module, Opts, Size, Name

  true = ets:insert(Name, {rings, {RingBuf, ServerRing, RingTable}}),

  {ok, {{one_for_one, 10, 10}, []}}.

%% @private
concat_tuples(T1, T2)->
  list_to_atom(atom_to_list(T1)++atom_to_list(T2)).
