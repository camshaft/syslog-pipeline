%% @doc Behaviour for emitters.
%%
%% Only one function needs to be implemented, <em>send/1</em>.
%% It receives a list of parsed events to be emitted to a backend.
-module(syslog_pipeline_emitter).

-callback send(Entries)
  -> ok
  | {error, term()}
  when Entries::[syslog_pipeline:entry()].
