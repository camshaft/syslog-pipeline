-module(syslog_pipeline_router).

-include("syslog_pipeline.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([compile/2]).
-export([match/2]).

%% TODO design the router

compile([Emitter|_Emitters], Messages)->
  {Emitter, Messages}.

match(_, _)->
  true.
