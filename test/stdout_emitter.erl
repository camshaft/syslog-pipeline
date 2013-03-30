-module (stdout_emitter).

-export([emit/1]).

emit(Messages)->
  io:format("~n~p~n", [Messages]).
