-module(syslog_pipeline_metrics).

-export([init/0]).
-export([submit_report/1]).
-export([do_report/0]).

init()->
  %% Rings
  folsom_metrics:new_histogram(ring_buffer_metric, uniform),
  folsom_metrics:new_histogram(server_ring_metric, uniform),

  %% Workers
  folsom_metrics:new_histogram(handle_time, uniform),
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
  [Mod:Function(make_message(Time, Key, Value)) || {Key, Value} <- do_report()],
  ok.

do_report()->
  [
    {<<"handle_time">>, syslog_pipeline:get_value(harmonic_mean, folsom_metrics:get_histogram_statistics(handle_time), -1)},
    {<<"frames_per_second">>, per_second(frames)},
    {<<"messages_per_second">>, per_second(valid_bodies)},
    {<<"events_per_second">>, per_second(events)},
    {<<"dropped_frames">>, folsom_metrics:get_metric_value(dropped_frames)},
    {<<"dropped_headers">>, folsom_metrics:get_metric_value(dropped_headers)},
    {<<"dropped_bodies">>, folsom_metrics:get_metric_value(dropped_bodies)},
    {<<"dropped_routes">>, folsom_metrics:get_metric_value(dropped_routes)},
    {<<"dropped_events">>, folsom_metrics:get_metric_value(dropped_events)},
    {<<"frame_time_avg">>, syslog_pipeline:get_value(harmonic_mean, folsom_metrics:get_histogram_statistics(frame_time), -1)},
    {<<"header_time_avg">>, syslog_pipeline:get_value(harmonic_mean, folsom_metrics:get_histogram_statistics(header_time), -1)},
    {<<"body_time_avg">>, syslog_pipeline:get_value(harmonic_mean, folsom_metrics:get_histogram_statistics(body_time), -1)},
    {<<"router_time_avg">>, syslog_pipeline:get_value(harmonic_mean, folsom_metrics:get_histogram_statistics(router_time), -1)}
  ].

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

per_second(Name)->
  Props = folsom_metrics:get_metric_value(Name),
  syslog_pipeline:get_value(one, Props, 0)/60.
