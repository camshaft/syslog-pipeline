-module(syslog_pipeline_worker_header).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record (state, {
  next = {syslog_pipeline, parse_body}
}).

start_link(Opts) ->
  Next = syslog_pipeline:get_value(next, Opts, {syslog_pipeline, parse_body}),
  gen_server:start_link(?MODULE, #state{next=Next}, []).

init(State) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, undef, State}.

handle_cast({handle, Frames}, #state{next={Module, Function}}=State) ->
  Messages = [folsom_metrics:histogram_timed_update(header_time,syslog_header,parse,[Frame]) || Frame <- Frames],
  Module:Function(Messages),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({next, Next}, State) ->
  {noreply, State#state{next=Next}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
