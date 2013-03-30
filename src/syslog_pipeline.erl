%%
%% syslog_pipeline.erl
%% syslog_pipeline entry point
%%
-module (syslog_pipeline).

-export ([start/0, start/2]).
-export ([stop/0, stop/1]).
-export([handle/1]).
-export([parse_header/1]).
-export([parse_body/1]).
-export([route_messages/1]).
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

parse_header(Frames)->
  do_work(syslog_pipeline_worker_header, cast, Frames, dropped_headers).

parse_body(Messages)->
  do_work(syslog_pipeline_worker_body, cast, Messages, dropped_bodies).

route_messages(Messages)->
  io:format("~p~n", [Messages]).
  % do_work(syslog_pipeline_worker_router, cast, Messages, dropped_routes).


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