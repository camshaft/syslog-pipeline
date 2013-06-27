-module(syslog_pipeline_app).

-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_, _) ->
  syslog_pipeline_sup:start_link().

stop(_) ->
  ok.
