%% Copyright
-module(sc_sup).
-author("dilzio").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).

%% API
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor callbacks
%% this sets up two child procs.  One is the superviser for elements, the other is the event manager
init([]) ->
  ElementSup = {sc_element_sup, {sc_element_sup, start_link, []}, permanent, 2000, supervisor, [sc_element]},
  EventManager = {sc_event, {sc_event, start_link, []}, permanent, 2000, worker, [sc_event]},
  Children = [ElementSup, EventManager],
  RestartStrategy= {one_for_one, 4, 3600},
  {ok, {RestartStrategy, Children}}.
