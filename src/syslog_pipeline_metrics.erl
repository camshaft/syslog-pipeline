-module(syslog_pipeline_metrics).

-export([init/0]).
-export([submit_report/1]).

init()->
  %% Frames
  folsom_metrics:new_spiral(frames),
  folsom_metrics:new_histogram(frame_time, uniform),
  folsom_metrics:new_counter(dropped_frames),
  %% Headers
  folsom_metrics:new_histogram(header_time, uniform),
  folsom_metrics:new_spiral(valid_headers),
  folsom_metrics:new_spiral(invalid_headers),
  folsom_metrics:new_counter(dropped_headers),
  %% Bodies
  folsom_metrics:new_histogram(body_time, uniform),
  folsom_metrics:new_spiral(valid_bodies),
  folsom_metrics:new_spiral(invalid_bodies),
  folsom_metrics:new_counter(dropped_bodies),
  %% Routes
  folsom_metrics:new_histogram(router_time, uniform),
  folsom_metrics:new_counter(dropped_routes),
  %% Events
  folsom_metrics:new_spiral(events),
  folsom_metrics:new_counter(dropped_events).

submit_report({Mod, Function})->
  Time = calendar:universal_time(),
  %% TODO add more metrics
  [Mod:Function(make_message(Time, Key, Value)) || {Key, Value} <- [
    {<<"frames_per_minute">>, per_minute(frames)},
    {<<"messages_per_minute">>, per_minute(valid_bodies)},
    {<<"events_per_minute">>, per_minute(events)},
    {<<"dropped_frames">>, folsom_metrics:get_metric_value(dropped_frames)},
    {<<"dropped_headers">>, folsom_metrics:get_metric_value(dropped_headers)},
    {<<"dropped_bodies">>, folsom_metrics:get_metric_value(dropped_bodies)},
    {<<"dropped_routes">>, folsom_metrics:get_metric_value(dropped_routes)}
  ]],
  ok.

make_message(Time, Key, Value)->
  [
    {priority, 40},
    {version, 1},
    {timestamp, Time},
    {hostname, <<"localhost">>},
    {app_name, <<"syslog_pipeline">>},
    {proc_id, <<"worker.1">>},
    {message_fields, [
      {<<"measure">>, Key},
      {<<"val">>, Value}
    ]}
  ].

per_minute(Name)->
  Props = folsom_metrics:get_metric_value(Name),
  syslog_pipeline:get_value(one, Props, 0).
