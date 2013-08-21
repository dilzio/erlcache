-module(hi_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%supervisor startup
start_link(Port) ->
    error_logger:info_msg("In hi_sup:start_link, about to call supervisor:start_link~n"),
    Ret = supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]),
    error_logger:info_msg("Started hi_sup on port ~p ~n", [Port]), 
    Ret.

%% spawn a new child process spec'ed in init/1
start_child() ->
    error_logger:info_msg("hi_sup:start_child() was entered.~n"), 
    Ret = supervisor:start_child(?SERVER, []),
    error_logger:info_msg("hi_sup:start_child() was called.~n"),
    Ret.

%%supervisor init -- args passed from line 15 call to supervisor:start_link/3
%%Port will be passed as an argument to any newly created children
init([Port]) ->
    %error_logger:info_msg("Entered hi_sup:init/1 Port: ~p~n", [Port]), 
    Server = {hi_server, {hi_server, start_link, [Port]}, permanent, 2000, worker, [hi_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
