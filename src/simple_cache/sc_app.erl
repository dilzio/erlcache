%% Copyright
-module(sc_app).
-author("dilzio").

-behaviour(application).

% application
-export([start/0, start/2, stop/1, bootstrap/0]).

-define(WAIT_FOR_RESOURCES, 2500).

bootstrap() ->
    application:start(sasl),
    mnesia:start(),
    application:start(resource_discovery),
    application:start(tcp_interface),
    application:start(http_interface),
    application:start(simple_cache).

% application callbacks
start(_Type, _Args) ->
  ok = ensure_contact(), %% if ensure_contact doesn't evaluate to ok, exception will be thrown and system won't start
  resource_discovery:add_local_resource(simple_cache, node()),
  resource_discovery:add_target_resource_type(simple_cache),
  resource_discovery:trade_resources(),
  timer:sleep(?WAIT_FOR_RESOURCES),
  sc_store:init(),
  case sc_element_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.
start() -> 
	start(arg1,arg1).

ensure_contact() ->
	DefaultNodes = ['contact1@localhost', 'contact2@localhost'],
	case get_env(simple_cache, contact_nodes, DefaultNodes) of
		[] ->
			{error, no_contact_nodes};
		ContactNodes ->
			ensure_contact(ContactNodes)
	end.
ensure_contact(ContactNodes) ->
	Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],  %% "The list of N where N is taken from ContactNodes where net_adm:ping(N) is pong"
	case Answering of
		[] ->
			{error, no_contact_nodes_reachable};
		_ -> 
			DefaultTime = 6000,
			WaitTime = get_env(simple_cache, wait_time, DefaultTime),
			wait_for_nodes(length(Answering),WaitTime)   
	end.
wait_for_nodes(MinNodes, WaitTime) ->  %% wait_for_nodes entrypoint.  Wait in 10 slices of WaitTime time
	Slices = 10,
	SliceTime = round(WaitTime/Slices),
	wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
	ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) -> %%wait loop that waits for nodes to connect
	case length(nodes()) > MinNodes of   %%MinNodes is the number of nodes that were available initially.  if nodes() returns > MinNodes, then we know were clustered and can return
		true ->
			ok;
		false ->
			timer:sleep(SliceTime),
			wait_for_nodes(MinNodes, SliceTime, Iterations -1)
	end.

get_env(AppName, Key, Default) ->
	case application:get_env(AppName, Key) of
		undefined -> Default;
		{ok, Value} -> Value
	end.

stop(_State) ->
  ok.
