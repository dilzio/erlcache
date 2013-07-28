%% Copyright
-module(sc_store).
-author("dilzio").

%% API
-export([init/0,
         insert/2,
         delete/1,
         lookup/1]).

-define(TABLE_ID, ?MODULE).

init() ->
  ets:new(?TABLE_ID, [public, named_table]),   %public tuple allows proc to be shared amongst multiple procs
  ok.

insert(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).   %by default first element of tuple is the key, this can be changed with an option in ets:new()

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of   % don't leak ets look up value to outside world
    [{Key, Pid}] -> {ok, Pid};
    [] -> {error, not_found}
  end.

delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).   %matches any 2-tuple with a second element of Pid variable




