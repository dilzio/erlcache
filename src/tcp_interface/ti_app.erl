-module(ti_app).

-behaviour(application).

-export([start/2, stop/1]).

%% this is the port for the listening socket which is passed to all newly spawned ti_server procs
-define(DEFAULT_PORT, 1155).

start(_StartType, _StartArgs) ->
    %% get port from config or use default
    Port = case application:get_env(tcp_interface, port) of
            {ok, P} -> P;
            undefined -> ?DEFAULT_PORT
           end,
    %% open socket
    {ok, LSock} =  gen_tcp:listen(Port, [{active, true}]), %% the active option stipulates that incoming socket traffic will be passed to process directly
    %% start top supervisor and if ok spawn a first child acceptor process
    case ti_sup:start_link(LSock) of
        {ok, Pid} ->
                ti_sup:start_child(),
                {ok, Pid};
        Other ->
                {error, Other}
    end.


stop(_State) ->
    ok.
                
