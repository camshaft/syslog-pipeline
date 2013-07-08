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
  application:start(syslog_pipeline),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(Name, Config) ->
  {ok, Ref} = syslog_pipeline:start_pipeline(Name, 10, syslog_message_keyvalue, []),
  [{server_ref, Ref}|Config].

end_per_group(_Name, _) ->
  ok.

basic(Config) ->
  Ref = proplists:get_value(server_ref, Config),
  DataDir = proplists:get_value(data_dir, Config),
  {ok, Buffer} = file:read_file(filename:join([DataDir,"basic.txt"])),
  <<>> = syslog_pipeline:handle(Ref, Buffer),
  ok.
