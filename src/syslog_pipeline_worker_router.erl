-module(syslog_pipeline_worker_router).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record (state, {
  adapters
}).

start_link(Opts) ->
  Adapters = syslog_pipeline:get_value(adapters, Opts, []),
  gen_server:start_link(?MODULE, #state{adapters=Adapters}, []).

init(State) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, undef, State}.

handle_cast({handle, Message}, #state{adapters=Adapters}=State) ->
  Results = [syslog_pipeline:convert(Message, Adapter) || Adapter <- Adapters, Adapter:is_valid(Message)],
  folsom_metrics:notify({events, length(Results)}),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
