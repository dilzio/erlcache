%% Copyright
-module(rd_sup).
-author("dilzio").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).

%% API call for starting the supervisor, abstracts the underlying impl call to supervisor:start_link
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks

%% this sets up two child procs.  One is the superviser for elements, the other is the event manager.
init([]) ->
  ElementSup = {resource_discovery, {resource_discovery, start_link, []}, permanent, 2000, supervisor, [resource_discovery]},
  Children = [ElementSup],
  RestartStrategy= {one_for_one, 4, 3600},
  {ok, {RestartStrategy, Children}}.


