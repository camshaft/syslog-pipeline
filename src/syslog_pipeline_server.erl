-module(syslog_pipeline_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get_body_parser/1]).
-export([set_body_parser/2]).
-export([get_emitters/1]).
-export([set_emitters/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB, ?MODULE).

-record(state, {}).

%% API.

%% @doc Start the syslog_pipeline_server gen_server.
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Return the pipeline's emitter.
-spec get_body_parser(syslog_pipeline:ref()) -> module().
get_body_parser(Ref) ->
  ets:lookup_element(?TAB, {body_parser, Ref}, 2).

%% @private
-spec set_body_parser(syslog_pipeline:ref(), module()) -> ok.
set_body_parser(Ref, Mod) ->
  gen_server:call(?MODULE, {set_body_parser, Ref, Mod}).

%% @doc Return the pipeline's emitters.
-spec get_emitters(syslog_pipeline:ref()) -> [{binary(), binary()}].
get_emitters(Ref) ->
  ets:lookup_element(?TAB, {emitters, Ref}, 2).

%% @private
-spec set_emitters(syslog_pipeline:ref(), module()) -> ok.
set_emitters(Ref, Emitters) ->
  gen_server:call(?MODULE, {set_emitters, Ref, Emitters}).

%% gen_server.

%% @private
init([]) ->
  {ok, #state{}}.

%% @private
handle_call({set_body_parser, Ref, Mod}, _, State) ->
  Response = ets:insert(?TAB, {{body_parser, Ref}, Mod}),
  {reply, Response, State};
handle_call({set_emitters, Ref, Emitters}, _, State) ->
  Response = ets:insert(?TAB, {{emitters, Ref}, Emitters}),
  {reply, Response, State};
handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
