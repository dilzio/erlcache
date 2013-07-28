%% Copyright
-module(die_please).
-author("dilzio").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SLEEP_TIME, (5000)).

-record(state, {}).

%% API
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% gen_server callbacks
init([]) ->
  {ok, #state{}, ?SLEEP_TIME}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(timeout, State) ->
  %i_want_to_die = right_now,
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



