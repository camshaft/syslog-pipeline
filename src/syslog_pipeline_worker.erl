-module(syslog_pipeline_worker).

% api

-export ([handle/2]).
-export ([handle/3]).

% private

-export ([execute/3]).

handle(Ref, Buffer) ->
  handle(Ref, Buffer, []).

handle(Ref, Buffer, Headers) ->
  %% We'll parse the octet frame in their process since it's super fast
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),

  %% Send off the frames to a worker
  spawn(?MODULE, execute, [Ref, Frames, Headers]),

  Buffer2.

-spec execute(syslog_pipeline:ref(), [binary()], [{atom(), binary()}]) -> [syslog_pipeline:entry()].
execute(Ref, Frames, Headers) ->
  %% Look up the env for our pipeline
  BodyParsers = syslog_pipeline_server:get_body_parsers(Ref),
  Emitters = syslog_pipeline_server:get_emitters(Ref),
  Filters = syslog_pipeline_server:get_filters(Ref),

  %% Parse the frames
  Entries = [parse(Frame, BodyParsers) || Frame <- lists:reverse(Frames)],

  %% Merge the headers into the Entries
  MergedEntries = merge_headers(Entries, Headers),

  %% Filter/transform the entries
  FilteredEntries = [filter(Entry, Filters) || Entry <- MergedEntries],

  %% Emit and expand the messages
  [emit(FilteredEntries, Emitter) || Emitter <- Emitters],

  %% Return the parsed frames
  Entries.

parse(Frame, BodyParsers) ->
  %% Strip the body of the \n so we don't get it in the parsed output
  Length = byte_size(Frame)-1,
  StrippedFrame = case Frame of
    <<SFrame:Length/binary, "\n">> ->
      SFrame;
    Other ->
      Other
  end,

  case syslog_header:parse(StrippedFrame) of
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

merge_headers(Entries, []) ->
  Entries;
merge_headers(Entries, [Header|Headers]) ->
  Entries2 = [set_header(Entry, Header) || Entry <- Entries],
  merge_headers(Entries2, Headers).

set_header({{_, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields}, {priority, Priority}) ->
  {{Priority, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields};
set_header({{Priority, _, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields}, {version, Version}) ->
  {{Priority, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields};
set_header({{Priority, Version, _, Hostname, AppName, ProcID, MessageID, Message}, Fields}, {timestamp, Timestamp}) ->
  {{Priority, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields};
set_header({{Priority, Version, Timestamp, _, AppName, ProcID, MessageID, Message}, Fields}, {hostname, Hostname}) ->
  {{Priority, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields};
set_header({{Priority, Version, Timestamp, Hostname, _, ProcID, MessageID, Message}, Fields}, {app_name, AppName}) ->
  {{Priority, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields};
set_header({{Priority, Version, Timestamp, Hostname, AppName, _, MessageID, Message}, Fields}, {proc_id, ProcID}) ->
  {{Priority, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields};
set_header({{Priority, Version, Timestamp, Hostname, AppName, ProcID, _, Message}, Fields}, {message_id, MessageID}) ->
  {{Priority, Version, Timestamp, Hostname, AppName, ProcID, MessageID, Message}, Fields};
set_header(Entry, _) ->
  Entry.

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
