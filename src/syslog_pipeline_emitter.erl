%% @doc Behaviour for emitters.
%%
%% Only one function needs to be implemented, <em>emit/1</em>.
%% It receives a list of parsed events from a syslog drain.
-module(syslog_pipeline_emitter).

-callback emit(Frame)
  -> any()
  when Frame::list([{binary(), binary()}]).
