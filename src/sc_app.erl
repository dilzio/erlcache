%% Copyright
-module(sc_app).
-author("dilzio").

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
  sc_store:init(),
  case sc_element_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.
