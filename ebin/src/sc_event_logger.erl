%% Copyright
-module(sc_event_logger).
-author("dilzio").

-behaviour(gen_event).



%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
  code_change/3]).

-record(state, {}).

%% gen_event callbacks
init(_Args) ->
  {ok, #state{}}.


handle_event({create, {Key, Value}}, State) ->
  error_logger:info_msg("Create called with Key:~s Value~s", [Key, Value]),
  {ok, State};

handle_event({replace, {Key, Value}}, State) ->
  error_logger:info_msg("Replace called with Key:~s Value~s", [Key, Value]),
  {ok, State};

handle_event({lookup, Key}, State) ->
  error_logger:info_msg("Lookup called with Key:~s", [Key]),
  {ok, State};

handle_event({delete, Key}, State) ->
  error_logger:info_msg("Delete called with Key:~s", [Key]),
  {ok, State}.


handle_call(_Request, State) ->
  {ok, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
