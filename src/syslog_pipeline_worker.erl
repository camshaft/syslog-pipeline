-module(syslog_pipeline_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record (state, {}).

start_link(_Opts) ->
  gen_server:start_link(?MODULE, #state{}, []).

init(State) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, undef, State}.

handle_cast({frames,Table,FrameIds}, State) ->
  [handle_frame(ring_buf:read(Table,Id)) || Id <- FrameIds],
  {noreply, State};
handle_cast(_Msg, State) ->
  io:format("~p~n", [_Msg]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_frame(Frame)->
  % io:format("~p~n", [Frame]),
  case syslog_header:parse(Frame) of
    {ok, Entry} ->
      % io:format("~p~n", [Entry]),
      case syslog_message_keyvalue:parse(syslog_pipeline:get_value(message, Entry, <<>>)) of
        {ok, Fields} ->
          ParsedEntry = [{message_fields, Fields}|Entry],
          % io:format("~p~n", [ParsedEntry]),
          %% TODO route the message
          folsom_metrics:notify(valid_bodies, 1),
          ParsedEntry;
        _ ->
          ok
      end;
    _ ->
      ok
  end.
