%% @doc Behaviour for body parsers.
%%
%% Only one function needs to be implemented, <em>parse/1</em>.

-module(syslog_pipeline_body_parser).

-callback parse(Body)
  -> {ok, syslog_pipeline:bin_proplist()}
  | {error, term()}
  when Body::binary().
