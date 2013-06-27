-module (stdout_emitter).

-export([send/1]).

send(Messages)->
  io:format("~n~p~n", [Messages]).
