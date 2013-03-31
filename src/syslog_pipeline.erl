%%
%% syslog_pipeline.erl
%% syslog_pipeline entry point
%%
-module (syslog_pipeline).

-export([start/0, start/2]).
-export([stop/0, stop/1]).
-export([handle/1]).
-export([parse_header/1]).
-export([parse_body/1]).
-export([route_message/1]).
-export([convert/2]).
-export([get_value/3]).


start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

start(_Type, _Args) ->
  syslog_pipeline_sup:start_link().

stop(_State) ->
  ok.

handle(Buffer)->
  do_work(syslog_pipeline_worker_frame, call, Buffer, dropped_frames).

parse_header(Frame)->
  do_work(syslog_pipeline_worker_header, cast, Frame, dropped_headers).

parse_body(Message)->
  do_work(syslog_pipeline_worker_body, cast, Message, dropped_bodies).

route_message(Message)->
  do_work(syslog_pipeline_worker_router, cast, Message, dropped_routes).

convert(Message, Worker)->
  do_work(Worker, cast, Message, dropped_events).

do_work(Pool, Fn, Messages, Metric)->
  case poolboy:checkout(Pool, false) of
    full ->
      folsom_metrics:notify({Metric, {inc, 1}});
    Worker ->
      Result = gen_server:Fn(Worker, {handle, Messages}),
      poolboy:checkin(Pool, Worker),
      Result
  end.

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.
