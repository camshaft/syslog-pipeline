-module (syslog_pipeline_stdout_emitter).

-include_lib("eunit/include/eunit.hrl").

-export([emit/1]).

emit(Messages)->
  io:format("~n~p~n", [Messages]).
