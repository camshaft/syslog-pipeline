-module (app_metric_expander).

-export([expand/1]).

expand({{_, _, _, _, <<"app">>, <<"web.",_>>, _, _}, Fields}=Message) ->
  %% Check to see if we have a 'measure' field
  case proplists:get_value(<<"measure">>, Fields) of
    undefined ->
      [];
    _ ->
      [Message]
  end;
expand(_) ->
  [].
