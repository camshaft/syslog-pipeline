-module(syslog_pipeline_metrics).

-export([init/0]).
-export([report/1]).
-export([dropped_messages/0]).
-export([frames_per_second/0]).
-export([messages_per_second/0]).
-export([events_per_second/0]).

init()->
  folsom_metrics:new_spiral(frames),
  folsom_metrics:new_histogram(frame_time, uniform),
  folsom_metrics:new_counter(dropped_frames),

  folsom_metrics:new_histogram(header_time, uniform),
  folsom_metrics:new_spiral(valid_headers),
  folsom_metrics:new_spiral(invalid_headers),
  folsom_metrics:new_counter(dropped_headers),

  folsom_metrics:new_histogram(body_time, uniform),
  folsom_metrics:new_spiral(valid_bodies),
  folsom_metrics:new_spiral(invalid_bodies),
  folsom_metrics:new_counter(dropped_bodies),

  folsom_metrics:new_histogram(router_time, uniform),
  folsom_metrics:new_counter(dropped_routes),

  folsom_metrics:new_spiral(events).

report(Name)->
  folsom_metrics:get_histogram_statistics(Name).

dropped_messages()->
  [
    {frames, folsom_metrics:get_metric_value(dropped_frames)},
    {headers, folsom_metrics:get_metric_value(dropped_headers)},
    {bodies, folsom_metrics:get_metric_value(dropped_bodies)},
    {routes, folsom_metrics:get_metric_value(dropped_routes)}
  ].

frames_per_second()->
  per_second(frames).

messages_per_second()->
  per_second(messages).
  
events_per_second()->
  per_second(events).

per_second(Name)->
  Props = folsom_metrics:get_metric_value(Name),
  syslog_pipeline:get_value(one, Props, 0)/60.
