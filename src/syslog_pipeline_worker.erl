-module(syslog_pipeline_worker).

-export ([execute/2]).

-include ("syslog_pipeline.hrl").

-spec execute(binary(), [{atom(), term()}]) -> [syslog_pipeline:entry()].
execute(Frames, Env) ->
  BodyParser = syslog_pipeline:get_value(body_parser, Env, syslog_message_keyvalue),
  Emitters = syslog_pipeline:get_value(emitters, Env, []),

  %% Parse the frames
  Entries = [parse(Frame, BodyParser) || Frame <- Frames],

  %% Emit and expand the messages
  [emit(Entries, Emitter) || Emitter <- Emitters],

  %% Return the parsed frames
  Entries.

parse(Frame, BodyParser) ->
  case syslog_header:parse(Frame) of
    {ok, Entry} ->
      try BodyParser:parse(syslog_pipeline:get_value(message, Entry, <<>>)) of
        {ok, Fields} ->
          [{message_fields, Fields}|Entry];
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
