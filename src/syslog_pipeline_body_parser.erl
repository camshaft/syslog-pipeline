%% @doc Behaviour for parsers.
%%
%% Only one function needs to be implemented, <em>parse/1</em>.
%% It receives a binary message recieved from a frame.
-module(syslog_pipeline_body_parser).

-callback parse(Message)
  -> list([{binary(), binary()}])
  when Message::binary().
