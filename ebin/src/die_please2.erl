%% Copyright
-module(die_please2).
-author("dilzio").

-export([go/0]).
-define(SLEEP_TIME, 5000).

go()->
  %%timer:sleep(?SLEEP_TIME),
  %%die = no.
  ok.