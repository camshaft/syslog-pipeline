%%
%% syslog_pipeline.erl
%% syslog_pipeline entry point
%%
-module (syslog_pipeline).

-export([start_pipeline/2]).
-export([stop_pipeline/1]).
-export([set_env/3]).
-export([get_value/3]).

%% @doc Start a syslog drain listener.
-spec start_pipeline(any(), any()) -> {ok, pid()}.
start_pipeline(Ref, Opts) ->
  {Ref, Opts}.

%% @doc Stop a listener.
-spec stop_pipeline(any()) -> ok.
stop_pipeline(_Ref) ->
  ok.

%% @doc Convenience function for setting an environment value.
%%
%% Allows you to update live an environment value; mainly used to
%% add/remove parsers and emitters
-spec set_env(any(), atom(), any()) -> ok.
set_env(_Ref, _Name, _Value) ->
  %% TODO
  % Opts = ranch:get_protocol_options(Ref),
  % Opts2 = [{Name, Value}|lists:keydelete(Name, 1, Opts)],
  % ok = ranch:set_protocol_options(Ref, Opts2).
  ok.


%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.
