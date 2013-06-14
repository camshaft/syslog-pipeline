%% @doc Behaviour for expanders.
%%
%% Only one function needs to be implemented, <em>expand/1</em>.
%% It receives a parsed entry and expands it into many/one/none entries understood
%% by an emitter.
-module(syslog_pipeline_expander).

-callback expand(Entry)
  -> [syslog_pipeline:entry()]
  | []
  when Entry::syslog_pipeline:entry().
