%%
%% syslog_pipeline.erl
%% syslog_pipeline entry point
%%
-module (syslog_pipeline).

-export([start_pipeline/2]).
-export([handle/2]).
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
-type ref() :: atom().

-export_type ([entry/0, bin_proplist/0, ref/0]).

-spec start_pipeline(atom(), [{atom(), term()}]) -> {ok, ref()}.
start_pipeline(Ref, Env) ->
  syslog_pipeline_sup:start_link(Ref, Env),
  {ok, Ref}.

-spec handle(ref(), binary()) -> binary().
handle(Ref, Buffer)->
  %% We'll parse the octet frame in their process since it's super fast
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),

  %% Look up the env for our pipeline
  {_, Env} = hd(ets:lookup(Ref, env)),

  %% Send off the frames to a worker
  %% TODO are there better ways to do this?:
  %%  - gen_server (might be too much overhead - we don't care if this crashes)
  %%  - ring buffer
  %%  - worker pool that handles casting well
  spawn(syslog_pipeline_worker, execute, [Frames, Env]),

  Buffer2.

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.
