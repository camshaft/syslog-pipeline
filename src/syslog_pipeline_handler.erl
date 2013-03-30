-module(syslog_pipeline_handler).

-include("syslog_pipeline.hrl").
-include_lib("eunit/include/eunit.hrl").

-export ([handle/2]).
%% @private
-export ([parse_frame/1]).
-export ([parse_body/3]).
-export ([compile_routes/2]).
-export ([exec_router/1]).

handle(Buffer, _Options=#pipeline_opts{feedback=true,parsers=Parsers,routes=Routes})->
  {_FrameTime, {Frames, Buffer2}} = timer:tc(syslog_octet_frame, parse, [Buffer]),
  {_ParseTime, Messages} = timer:tc(?MODULE, parse_frame, [Frames]),
  {_ParseBodyTime, ValidMessages} = timer:tc(?MODULE, parse_body, [Messages, Parsers, []]),
  {_CompileTime, CompiledRoutes} = timer:tc(?MODULE, compile_routes, [Routes, ValidMessages]),
  {_RouteTime, _} = timer:tc(?MODULE, exec_router, [CompiledRoutes]),
  Total = _FrameTime+_ParseTime+_ParseBodyTime+_CompileTime+_RouteTime,
  ?debugFmt("~nFrame: ~p~nParse: ~p~nBody: ~p~nCompile: ~p~nRoute: ~p~n~nTotal time: ~p~n",[_FrameTime, _ParseTime, _ParseBodyTime, _CompileTime, _RouteTime, Total]),
  Buffer2;
handle(Buffer, _Options=#pipeline_opts{parsers=Parsers,routes=Routes})->
  {Frames, Buffer2} = syslog_octet_frame:parse(Buffer),
  Messages = parse_frame(Frames),
  ValidMessages = parse_body(Messages, Parsers, []),
  CompiledRoutes = compile_routes(Routes, ValidMessages),
  exec_router(CompiledRoutes),
  Buffer2.

parse_frame(Frames)->
  [syslog_header:parse(Frame) || Frame <- Frames].

parse_body([], _, ValidMessages)->
  ValidMessages;
parse_body([{ok, Message}|Messages], Parsers, ValidMessages)->
  case exec_parser(syslog_pipeline:get_value(message, Message, <<>>), Parsers) of
    {ok, MessageFields} ->
      parse_body(Messages, Parsers, [[{message_fields,MessageFields}|Message]|ValidMessages]);
    _ ->
      parse_body(Messages, Parsers, ValidMessages)
  end;
parse_body([_|Messages], Parsers, ValidMessages)->
  parse_body(Messages, Parsers, ValidMessages).

exec_parser(_, [])->
  error;
exec_parser(Body, [Parser|Parsers])->
  case catch Parser:parse(Body) of
    {ok, MessageFields} ->
      {ok, MessageFields};
    _ ->
      exec_parser(Body, Parsers)
  end.

compile_routes(Routes, Messages)->
  [syslog_pipeline_router:compile(Routes, Message) || Message <- Messages, syslog_pipeline_router:match(Routes, Message)].

exec_router([])->
  ok;
exec_router([{Emitter, Messages}|Routes])->
  catch Emitter:emit(Messages),
  exec_router(Routes).
