
-define(PRINT(Var), io:format("~p~n", [Var])).

-record(pipeline_opts, {
  parsers :: [module()],
  mappers :: [module()],
  routes :: [module()],
  feedback :: boolean()
}).
