-module (riemann_emitter).

-export([send/1]).

-define (GREGORIAN_SECONDS, 62167219200).

send(Messages)->
  Events = [riemann:event(convert(Message)) || Message <- Messages],
  riemann:send(Events).

convert({{_, _, Timestamp, Hostname, _, _, _, _}, Fields}) ->
  [
    {time, datetime_to_unix(Timestamp)},
    {host, Hostname},
    {service, proplists:get_value(<<"measure">>, Fields)},
    {metric, proplists:get_value(<<"val">>, Fields)},
    {description, proplists:get_value(<<"units">>, Fields)},
    {state, proplists:get_value(<<"state">>, Fields)},
    {tags, [proplists:get_value(<<"request_id">>, Fields)]}
  ].

datetime_to_unix(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - ?GREGORIAN_SECONDS.
