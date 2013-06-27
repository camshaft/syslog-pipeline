-module (identity_expander).

-export([expand/1]).

expand(Message)->
  [Message].
