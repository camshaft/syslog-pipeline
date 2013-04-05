-module(pipeline_SUITE).

-include_lib("common_test/include/ct.hrl").


%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export ([basic/1]).

all() ->
  [{group, pipeline}].

groups() ->
  [{pipeline, [
    basic
  ]}].

init_per_suite(Config) ->
  ok = application:start(folsom),
  {ok, _} = syslog_pipeline:start_pipeline(test, 512, 512),
  Config.

end_per_suite(_Config) ->
  ok = application:stop(folsom),
  ok.

init_per_group(_, Config) ->
  Config.

end_per_group(_Name, _) ->
  ok.

basic(_Config)->
  Buffer = <<"182 <40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_5m val=0.00\n183 <40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_15m val=0.00\n">>,
  <<>> = syslog_pipeline:handle(test, Buffer),
  ok.
