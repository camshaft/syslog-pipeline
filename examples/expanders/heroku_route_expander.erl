-module (heroku_route_expander).

-export([expand/1]).

expand({{_, _, _, _, <<"heroku">>, <<"router">>, _, _}=Headers, Fields}) ->
  RequestID = proplists:get_value(<<"request_id">>, Fields),
  iterate(Headers, RequestID, Fields, []);
expand(_) ->
  [].

iterate(_, _, [], Acc) ->
  Acc;
iterate(Headers, RequestID, [Field|Fields], Acc) ->
  case metric(Field) of
    undefined ->
      iterate(Headers, RequestID, Fields, Acc);
    ExpandedField ->
      Message = {Headers, [{<<"request_id">>, RequestID}|ExpandedField]},
      iterate(Headers, RequestID, Fields, [Message|Acc])
  end.

metric({<<"method">>, Method}) ->
  [
    {<<"measure">>, <<"method">>},
    {<<"val">>, 1},
    {<<"units">>, <<"requests">>},
    {<<"state">>, Method}
  ];
metric({<<"path">>, Path}) ->
  [
    {<<"measure">>, <<"path">>},
    {<<"val">>, 1},
    {<<"units">>, <<"requests">>},
    {<<"state">>, Path}
  ];
metric({<<"status">>, StatusCode}) ->
  [
    {<<"measure">>, <<"status_code">>},
    {<<"val">>, 1},
    {<<"units">>, <<"requests">>},
    {<<"state">>, StatusCode}
  ];
metric({<<"queue">>, Queue}) ->
  [
    {<<"measure">>, <<"queue">>},
    {<<"val">>, Queue},
    {<<"units">>, <<"requests">>}
  ];
metric({<<"connect">>, Connect}) ->
  [
    {<<"measure">>, <<"connect">>},
    {<<"val">>, strip_ms(Connect)},
    {<<"units">>, <<"ms">>}
  ];
metric({<<"service">>, Service}) ->
  [
    {<<"measure">>, <<"service">>},
    {<<"val">>, strip_ms(Service)},
    {<<"units">>, <<"ms">>}
  ];
metric({<<"wait">>, Wait}) ->
  [
    {<<"measure">>, <<"wait">>},
    {<<"val">>, strip_ms(Wait)},
    {<<"units">>, <<"ms">>}
  ];
metric({<<"bytes">>, Bytes}) ->
  [
    {<<"measure">>, <<"bytes_out">>},
    {<<"val">>, Bytes},
    {<<"units">>, <<"bytes">>}
  ];
metric(_) ->
  undefined.

strip_ms(<<Val:1/binary,"ms">>) ->
  Val;
strip_ms(<<Val:2/binary,"ms">>) ->
  Val;
strip_ms(<<Val:3/binary,"ms">>) ->
  Val;
strip_ms(<<Val:4/binary,"ms">>) ->
  Val;
strip_ms(<<Val:5/binary,"ms">>) ->
  Val;
strip_ms(<<Val:6/binary,"ms">>) ->
  Val;
strip_ms(<<Val:7/binary,"ms">>) ->
  Val;
strip_ms(<<Val:8/binary,"ms">>) ->
  Val;
strip_ms(<<Val:9/binary,"ms">>) ->
  Val;
strip_ms(_) ->
  <<>>.
