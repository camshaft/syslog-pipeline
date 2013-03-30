-module(syslog_pipeline_worker_body).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% @private
-export([parse_body/3]).

-record (state, {
  parsers = [syslog_message_keyvalue],
  next = {syslog_pipeline, route_messages}
}).

start_link(Opts) ->
  Next = syslog_pipeline:get_value(next, Opts, {syslog_pipeline, route_messages}),
  gen_server:start_link(?MODULE, #state{next=Next}, []).

init(State) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, undef, State}.

handle_cast({handle, Messages}, #state{parsers=Parsers, next={Module,Function}}=State) ->
  ValidMessages = folsom_metrics:histogram_timed_update(body_time,?MODULE,parse_body,[Messages, Parsers, []]),
  folsom_metrics:notify({messages, length(ValidMessages)}),
  Module:Function(ValidMessages),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({next, Next}, State) ->
  {noreply, State#state{next=Next}};
handle_info({parsers, Parsers}, State) ->
  {noreply, State#state{parsers=Parsers}};
handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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
    E ->
      io:format("~p~n", [E]),
      exec_parser(Body, Parsers)
  end.
