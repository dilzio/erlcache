-module(hi_app).

-behaviour(application).

-export([start/2, stop/1]).

%% this is the port for the listening socket which is passed to all newly spawned ti_server procs
-define(DEFAULT_PORT, 1157).

start(_StartType, _StartArgs) ->
    %% get port from config or use default
    Port = case application:get_env(http_interface, port) of
            {ok, P} -> P;
            undefined -> ?DEFAULT_PORT
           end,
    %% start top supervisor 
    error_logger:info_msg("In hi_app:start, about to call hi_sup:start_link~n"), 
    case hi_sup:start_link(Port) of
        {ok, Pid} ->
            error_logger:info_msg("In hi_app:start, returned from call to hi_sup:start_link with {ok, Pid}~n"), 
            {ok, Pid};
        Other ->
            error_logger:info_msg("In hi_app:start, returned from call to hi_sup:start_link with Error: ~p~n", [Other]), 
            {error, Other}
    end.

stop(_State) ->
    ok.
