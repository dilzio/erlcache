-module(gws_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/4, start_child/1]).

%% Supervisor callbacks 
-export([init/1]).


%% API functions
start_link(Callback, IP, Port, UserArgs) ->
    {ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, Port, UserArgs]),
    start_child(Pid),
    {ok, Pid}.

%starts a new gws_server child process using the spec.  Server is the PID of the supervisor process which should be used for the child
start_child(Server) ->
    supervisor:start_child(Server, []).

%% Supervisor Callbacks
init([Callback, IP, Port, UserArgs]) ->
    BasicSockOpts = [binary,         %% binary option means incoming data is delivered as binaries not strings
                     {active, false}, %% socket opened in passive mode..flow control is pull mode
                     {packet, http_bin}, %% convenience option...data will be passed to callback preformatted for http
                     {reuseaddr, true}],  %% allow for resuse without having to wait for OS kernel to time out the socket
    SockOpts = case IP of 
                    undefined -> BasicSockOpts;
                    _         -> [{ip, IP}|BasicSockOpts]
               end,
    {ok, LSock} = gen_tcp:listen(Port, SockOpts),
    Server = {gws_server, {gws_server, start_link, [Callback, LSock, UserArgs]}, temporary, brutal_kill, worker, [gws_server]},
    RestartStrategy = {simple_one_for_one, 1000, 3600},
    {ok, {RestartStrategy, [Server]}}.
