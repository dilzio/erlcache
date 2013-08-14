-module(ti_sup).

-behavior(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%supervisor startup
start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

%% spawn a new child process spec'ed in init/1
start_child() ->
    supervisor:start_child(?SERVER, []).

%%supervisor init -- args passed from line 15 call to supervisor:start_link/3
%%LSock (listening socket) will be passed as an argument to any newly created children
init([LSock]) ->
    Server = {ti_server, {ti_server, start_link, [LSock]}, temporary, brutal_kill, worker, [ti_server]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
