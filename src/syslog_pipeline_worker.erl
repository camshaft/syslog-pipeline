-module(syslog_pipeline_worker).

% api

-export ([handle/2]).

% private

-export ([execute/2]).

handle(Ref, Buffer) ->
  %% We'll parse the octet frame in their process since it's super fast
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),

  %% Send off the frames to a worker
  spawn(?MODULE, execute, [Ref, Frames]),

  Buffer2.

-spec execute(syslog_pipeline:ref(), [binary()]) -> [syslog_pipeline:entry()].
execute(Ref, Frames) ->
  %% Look up the env for our pipeline
  BodyParsers = syslog_pipeline_server:get_body_parsers(Ref),
  Emitters = syslog_pipeline_server:get_emitters(Ref),
  Filters = syslog_pipeline_server:get_filters(Ref),

  %% Parse the frames
  Entries = [parse(Frame, BodyParsers) || Frame <- lists:reverse(Frames)],

  %% Filter/transform the entries
  FilteredEntries = [filter(Entry, Filters) || Entry <- Entries],

  %% Emit and expand the messages
  [emit(FilteredEntries, Emitter) || Emitter <- Emitters],

  %% Return the parsed frames
  Entries.

parse(Frame, BodyParsers) ->
  case syslog_header:parse(Frame) of
    {ok, {_, _, _, _, _, _, _, Body} = Headers} ->
      case parse_body(Body, BodyParsers) of
        {ok, Fields} ->
          {Headers, Fields};
        _ ->
          error
      end;
    _ ->
      error
  end.

parse_body(error, []) ->
  error;
parse_body(Body, [BodyParser|BodyParsers]) ->
  case catch BodyParser:parse(Body) of
    {ok, Fields} ->
      {ok, Fields};
    _ ->
      parse_body(Body, BodyParsers)
  end.

filter(Entry, []) ->
  Entry;
filter(Entry, [Filter|Filters]) ->
  case catch Filter:filter(Entry) of
    {ok, FilteredEntry} ->
      filter(FilteredEntry, Filters);
    _ ->
      error
  end.

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
