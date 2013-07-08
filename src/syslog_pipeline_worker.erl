-module(syslog_pipeline_worker).

-export ([start_pool/4]).
-export ([start_pool/5]).
-export ([handle/2]).

-export ([start_link/1]).
-export ([loop/1]).

start_pool(Ref, NumWorkers, BodyParser, Emitters) ->
  start_pool(Ref, NumWorkers, NumWorkers+1000, BodyParser, Emitters).

start_pool(Ref, NumWorkers, MaxWorkers, BodyParser, Emitters) ->
  syslog_pipeline_server:set_body_parser(Ref, BodyParser),
  syslog_pipeline_server:set_emitters(Ref, Emitters),
  pooler:new_pool([
    {name, Ref},
    {max_count, MaxWorkers},
    {init_count, NumWorkers},
    {start_mfa, {?MODULE, start_link, [Ref]}}
  ]).

handle(Ref, Buffer) ->
  %% We'll parse the octet frame in their process since it's super fast
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),

  Worker = pooler:take_member(Ref),

  %% Send off the frames to a worker
  Worker ! {execute, Frames},

  Buffer2.

-spec start_link(syslog_pipeline:ref()) -> ok.
start_link(Ref) ->
  Pid = spawn_link(?MODULE, loop, [Ref]),
  {ok, Pid}.

loop(Ref) ->
  receive
    {execute, Frames} ->
      execute(Ref, Frames),
      pooler:return_member(Ref, self()),
      ?MODULE:loop(Ref);
    _ ->
      ?MODULE:loop(Ref)
  end.

-spec execute(syslog_pipeline:ref(), [binary()]) -> [syslog_pipeline:entry()].
execute(Ref, Frames) ->
  %% Look up the env for our pipeline
  BodyParser = syslog_pipeline_server:get_body_parser(Ref),
  Emitters = syslog_pipeline_server:get_emitters(Ref),

  %% Parse the frames
  Entries = [parse(Frame, BodyParser) || Frame <- Frames],

  %% Emit and expand the messages
  [emit(Entries, Emitter) || Emitter <- Emitters],

  %% Return the parsed frames
  Entries.

parse(Frame, BodyParser) ->
  case syslog_header:parse(Frame) of
    {ok, {_, _, _, _, _, _, _, Body} = Headers} ->
      try BodyParser:parse(Body) of
        {ok, Fields} ->
          {Headers, Fields};
        _ ->
          error
      catch _:_ ->
        error
      end;
    _ ->
      error
  end.

emit(Entries, {Emitter, []}) ->
  Emitter:send(Entries);
emit(Entries, {Emitter, Expanders}) ->
  Entries2 = expand_entries(Entries, Expanders, []),
  Emitter:send(Entries2).

expand_entries([], _, Expanded) ->
  Expanded;
expand_entries([Entry|Entries], Expanders, Expanded) ->
  ExpandedEntry = expand(Entry, Expanders, []),
  expand_entries(Entries, Expanders, ExpandedEntry++Expanded).

expand(error, _, Entries) ->
  Entries;
expand(_Entry, [], Entries) ->
  Entries;
expand(Entry, [Expander|Expanders], Entries) ->
  Entries2 = try Expander:expand(Entry) catch
    _:_ -> []
  end ++ Entries,
  expand(Entry, Expanders, Entries2).
