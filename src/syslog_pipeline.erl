%%
%% syslog_pipeline.erl
%% syslog_pipeline entry point
%%
-module (syslog_pipeline).

-export([start_pipeline/3]).
-export([start_pipeline/4]).
-export([handle/2]).
-export([set_body_parsers/2]).
-export([set_emitters/2]).

-type entry() :: [entry_line()].
-type entry_line() ::
        {message_fields, bin_proplist()}
      | {priority, pos_integer()}
      | {version, pos_integer()}
      | {timestamp, calendar:datetime()}
      | {hostname, binary()}
      | {app_name, binary()}
      | {proc_id, binary()}
      | {message_id, binary() | undefined}
      | {message, binary()}.
-type bin_proplist() :: [{binary(), binary()}].
-type expander_list() :: [{module(), [module()]}].
-type ref() :: atom().

-export_type ([entry/0, bin_proplist/0, ref/0]).

-spec start_pipeline(ref(), [module()], expander_list()) -> {ok, ref()}.
start_pipeline(Ref, BodyParser, Emitters) when is_atom(BodyParser) ->
  start_pipeline(Ref, [BodyParser], Emitters);
start_pipeline(Ref, BodyParsers, Emitters) ->
  start_pipeline(Ref, BodyParsers, [], Emitters).

-spec start_pipeline(ref(), [module()], [module()], expander_list()) -> {ok, ref()}.
start_pipeline(Ref, BodyParsers, Filters, Emitters) ->
  syslog_pipeline_server:set_body_parsers(Ref, BodyParsers),
  syslog_pipeline_server:set_emitters(Ref, Emitters),
  syslog_pipeline_server:set_filters(Ref, Filters),
  {ok, Ref}.

-spec handle(ref(), binary()) -> binary().
handle(Ref, Buffer) ->
  syslog_pipeline_worker:handle(Ref, Buffer).

set_body_parsers(Ref, Mods) ->
  syslog_pipeline_server:set_body_parsers(Ref, Mods).

set_emitters(Ref, Emitters) ->
  syslog_pipeline_server:set_emitters(Ref, Emitters).
