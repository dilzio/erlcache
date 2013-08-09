%% Copyright
-module(resource_discovery).
-author("dilzio").

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_target_resource_type/1,
         add_local_resource/2,
         fetch_resources/1,
         trade_resources/0
         ]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, bootstrap/0]).

-define(SERVER, ?MODULE).


-record(state, {target_resource_types,     %% resources this node is looking for
                local_resource_tuples,     %% resources this node has
                found_resource_tuples}).   %% resources that have been found (union of local node and remote node resources)

%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).   %% ServerName, Module, Args, Options

add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Instance) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources,Type},infinity).

trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).

bootstrap() ->
	start_link(),
	add_local_resource(mylocaltype1, myinstance1),
	add_local_resource(mylocaltype1, myinstance2),
	add_local_resource(mylocaltype1, myinstance3),
	add_target_resource_type(mylocaltype1).

%% gen_server callbacks

init(_Args) ->
  {ok, #state{target_resource_types = [],
              local_resource_tuples = dict:new(),
              found_resource_tuples = dict:new()}}.

handle_cast({add_target_resource_type, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  {noreply, State#state{target_resource_types = NewTargetTypes}};

handle_cast({add_local_resource,{Type,Instance}}, State) ->
  UpdatedLocalResourcesDict = add_resource(Type, Instance, State#state.local_resource_tuples),
  UpdatedFoundResourcesDict = add_resource(Type, Instance, State#state.found_resource_tuples),
  U1 = State#state{local_resource_tuples = UpdatedLocalResourcesDict},
  U2 = U1#state{found_resource_tuples = UpdatedFoundResourcesDict},
  {noreply, U2};

handle_cast(trade_resources, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  AllNodes = [node() | nodes()],
  lists:foreach(fun(Node) -> gen_server:cast({?SERVER, Node}, {trade_resources, {node(), ResourceTuples}}) end, AllNodes),
  {noreply, State};

handle_cast({trade_resources, {ReplyTo, Remotes}}, State) ->
  Locals = State#state.local_resource_tuples,
  TargetTypes = State#state.target_resource_types,
  OldFound = State#state.found_resource_tuples,  
  FilteredRemotes =resources_for_types(TargetTypes, Remotes),
  NewFound = add_resources(FilteredRemotes, OldFound),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      gen_server:cast({?SERVER, ReplyTo}, {trade_resources, {noreply, Locals}})
  end,
  {noreply, State#state{found_resource_tuples = NewFound}}.

handle_call({fetch_resources, Type}, _From, State) -> 
  	Response = dict:find(Type, State#state.local_resource_tuples),
	case Response of 
	  error ->
		  	{reply,dict:find(Type, State#state.found_resource_tuples), State};
  	   _->
		    {reply, Response, State}
	 end.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% internal functions
add_resource(Type, Resource, ResourceTuples) ->
  case dict:find(Type, ResourceTuples) of
    {ok, ResourceList} ->
        NewList = [Resource | lists:delete(Resource, ResourceList)],
        dict:store(Type, NewList, ResourceTuples);  %returns a new dict
    error ->
        dict:store(Type, [Resource], ResourceTuples)
  end.

add_resources([{Type, Resource} | T], ResourceTuples) ->  
  add_resources(T, add_resource(Type, Resource, ResourceTuples)); 
add_resources([], ResourceTuples) ->
  ResourceTuples.

resources_for_types(Types, ResourceTuples) ->
  Fun =
    fun(Type, Acc) ->
      case dict:find(Type, ResourceTuples) of
        {ok, List} ->
            [{Type, Instance} || Instance <- List] ++ Acc;
        error ->
          Acc
      end
     end,
  lists:foldl(Fun, [], Types).
