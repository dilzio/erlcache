%% element supervisor process which acts as a factory for element processes
-module(sc_element_sup).
-author("dilzio").

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2
       ]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).
%% API call for starting the supervisor, abstracts the underlying impl call to supervisor:start_link
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% API call for starting a child element process...abstracts away the underlying call to supervisor:start_child
start_child(Value, LeaseTime) ->
  supervisor:start_child(?SERVER, [Value, LeaseTime]).

%% supervisor callbacks

%%specs out the child element process params for when start_child is called as well as a Restart strategy.
init([]) ->
  Element = {sc_element, {sc_element, start_link, []}, temporary, brutal_kill, worker, [sc_element]},
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
