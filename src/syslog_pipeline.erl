%%
%% syslog_pipeline.erl
%% syslog_pipeline entry point
%%
-module (syslog_pipeline).

-export([start_pipeline/4]).
-export([handle/2]).
-export([set_body_parser/2]).
-export([set_emitters/2]).
-export([get_value/3]).

-type entry() :: [entry_line()].
-type entry_line() ::
        {message_fields, bin_proplist()}
      | {priority, pos_integer()}
      | {version, pos_integer()}
      | {timestamp, calendar:datetime()}
      | {hostname, binary()}
      | {app_name, binary()}
      | {proc_id, binary()}
      | {message_id, binary() | undefined}
      | {message, binary()}.
-type bin_proplist() :: [{binary(), binary()}].
-type expander_list() :: [{module(), [module()]}].
-type ref() :: atom().

-export_type ([entry/0, bin_proplist/0, ref/0]).

-spec start_pipeline(ref(), pos_integer(), module(), expander_list()) -> {ok, ref()}.
start_pipeline(Ref, NumWorkers, BodyParser, Emitters) ->
  supervisor:start_child(syslog_pipeline_sup, child_spec(Ref, NumWorkers, BodyParser, Emitters)).

-spec child_spec(ref(), pos_integer(), module(), expander_list()) -> supervisor:child_spec().
child_spec(Ref, NumWorkers, BodyParser, Emitters) ->
  {{syslog_pipeline_worker_sup, Ref}, {syslog_pipeline_worker_sup, start_link, [
    Ref, NumWorkers, BodyParser, Emitters
  ]}, permanent, 5000, supervisor, [syslog_pipeline_worker_sup]}.

-spec handle(ref(), binary()) -> binary().
handle(Ref, Buffer) ->
  %% We'll parse the octet frame in their process since it's super fast
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),

  %% Get a worker pid
  Worker = syslog_pipeline_worker_sup:get_worker(Ref),

  %% Send off the frames to a worker
  Worker ! {execute, Frames},

  Buffer2.

set_body_parser(Ref, Mod) ->
  syslog_pipeline_server:set_body_parser(Ref, Mod).

set_emitters(Ref, Emitters) ->
  syslog_pipeline_server:set_emitters(Ref, Emitters).

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.
