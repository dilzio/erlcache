%% Copyright
-module(sc_element).
-author("dilzio").

-behaviour(gen_server).

%% API
-export([start_link/2,
         create/1,
         create/2,
         fetch/1,
         replace/2,
         delete/1
        ]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (20)).

-record(state, {value, lease_time, start_time}).

%% API
start_link(Value, LeaseTime) ->
  gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
  sc_element_sup:start_child(Value, LeaseTime).

create(Value) ->
  sc_element_sup:start_child(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

replace(Pid, Value) ->
  gen_server:cast(Pid, {replace, Value}).   % first argument is the pid of the process gen_server should call, second is the Request parameter
                                            % which is passed down to the callback function in the first parameter

delete(Pid) ->
  gen_server:cast(Pid, delete).

%% gen_server callbacks
init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  {ok,
   #state{value = Value,
          lease_time = LeaseTime,
          start_time = StartTime},
   time_left(StartTime, LeaseTime)}.

time_left(_StartTime, infinity) ->
   infinity;

time_left(StartTime, LeaseTime) ->
  Now = calendar:local_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
  TimeElapsed = CurrentTime - StartTime,
  case LeaseTime - TimeElapsed of
    Time when Time =< 0 -> 0;
    Time -> Time * 1000
  end.

handle_call(fetch, _From, State) ->
  #state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,        %map the state record fields into variables
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft}.  % reply atom, reply status/value tuple, state record unchanged,  updated timeout

handle_cast({replace, Value}, State) ->
  #state{lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {noreply, State#state{value = Value}, TimeLeft};

handle_cast(delete, State) ->
  {stop, normal, State}.

handle_info(timeout, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  sc_store:delete(self()),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



