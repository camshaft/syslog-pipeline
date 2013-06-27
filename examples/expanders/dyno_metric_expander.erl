-module (dyno_metric_expander).

-export([expand/1]).

expand({{_, _, _, _, <<"heroku">>, <<"web.",_>>, _, _}, _}=Message) ->
  [Message];
expand(_) ->
  [].
