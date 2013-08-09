%% Copyright
-module(rd_app).
-author("dilzio").

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
  case rd_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.


stop(_State) ->
  ok.
