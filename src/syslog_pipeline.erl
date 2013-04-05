%%
%% syslog_pipeline.erl
%% syslog_pipeline entry point
%%
-module (syslog_pipeline).

-export([start_pipeline/3]).
-export([handle/2]).
-export([get_value/3]).

start_pipeline(Name, HandleSize, ServerSize) ->
  syslog_pipeline_sup:start_link(Name, HandleSize, ServerSize),
  {ok, Name}.

handle(Ref, Buffer)->
  {_, {RingBuf, ServerRing, RingTable}} = hd(ets:lookup(Ref, rings)),
  {Frames, Buffer2} = folsom_metrics:histogram_timed_update(frame_time,syslog_octet_frame,parse,[Buffer]),
  {ok, Ids} = folsom_metrics:histogram_timed_update(ring_buffer_metric,ring_buf,add_all,[RingBuf,Frames]),
  folsom_metrics:histogram_timed_update(server_ring_metric,server_ring,transaction,[ServerRing,fun
    (Pid) -> gen_server:cast(Pid, {frames, RingTable, Ids})
  end]),
  Buffer2.

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.
