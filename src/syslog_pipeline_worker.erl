-module(syslog_pipeline_worker).

-export ([start_link/1]).
-export ([loop/1]).

-spec start_link(syslog_pipeline:ref()) -> ok.
start_link(Ref) ->
  Pid = spawn_link(?MODULE, loop, [Ref]),
  {ok, Pid}.

loop(Ref) ->
  receive
    {execute, Frames} ->
      execute(Ref, Frames),
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
