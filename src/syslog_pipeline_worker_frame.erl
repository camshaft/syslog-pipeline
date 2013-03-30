-module(syslog_pipeline_worker_frame).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("syslog_pipeline.hrl").

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record (state, {
  next = {syslog_pipeline, parse_header}
}).

start_link(Opts) ->
  Next = syslog_pipeline:get_value(next, Opts, {syslog_pipeline, parse_header}),
  gen_server:start_link(?MODULE, #state{next=Next}, []).

init(State) ->
  {ok, State}.

handle_call({handle, Buffer}, _From, State) ->
  {Frames, Buffer2} = folsom_metrics:histogram_timed_update(frame_time,syslog_octet_frame,parse,[Buffer]),
  self() ! {next, Frames},
  {reply, Buffer2, State};
handle_call(_Request, _From, State) ->
  {reply, undef, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({set_next, Next}, State) ->
  {noreply, State#state{next=Next}};
handle_info({next, Frames}, #state{next={Module, Function}}=State) ->
  folsom_metrics:notify({frames, length(Frames)}),
  Module:Function(Frames),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
