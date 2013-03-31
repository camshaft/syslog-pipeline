%% @doc Behaviour for emitters.
%%
%% Only one function needs to be implemented, <em>send/1</em>.
%% It receives a list of parsed events from a syslog drain.
-module(syslog_pipeline_emitter).

-callback send(Events)
  -> any()
  when Events::list([any()]).
