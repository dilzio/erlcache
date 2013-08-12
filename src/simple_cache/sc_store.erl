%% Copyright
-module(sc_store).
-author("dilzio").

%% API
-export([init/0,
         insert/2,
         delete/1,
         lookup/1]).

-define(TABLE_ID, ?MODULE).
-record(key_to_pid, {key, pid}).

init() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:start(),
  error_logger:info_msg("Called Mnesia:start()~n"),
  {ok, CacheNodes} = resource_discovery:fetch_resources(simple_cache),
  error_logger:info_msg("CacheNodes was: ~p~n", [CacheNodes]),
  dynamic_db_init(lists:delete(node(), CacheNodes)).

dynamic_db_init([]) ->
  mnesia:create_table(key_to_pid, 	
					  [{index, [pid]},  %% declare index on pid column
					   {attributes, record_info(fields, key_to_pid)}
					  ]);

dynamic_db_init(CacheNodes) ->
    add_extra_nodes(CacheNodes).

-define(WAIT_FOR_TABLES, 5000).

add_extra_nodes([Node|T]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} ->
            mnesia:add_table_copy(key_to_pid, node(), ram_copies),
            error_logger:info_msg("Node was: ~p~n", [Node]),
            Tables = mnesia:system_info(tables),
            mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
        _ ->
            add_extra_nodes(T)
    end.
insert(Key, Pid) ->
  	Resp = mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}),
    error_logger:info_msg("Wrote Key:~s Pid:~s to mnesia to key_to_pid table ~n", [Key,pid_to_list(Pid)]),
    Resp.

lookup(Key) ->
    %% because table is a set can get 0 or 1 answers
	case mnesia:dirty_read(key_to_pid, Key) of
		[{key_to_pid, Key, Pid}] ->  
			case is_pid_alive(Pid) of
					true -> {ok, Pid};
					false -> {error, not_found}
			end;
		[]						 -> {error, not_found}
	end.

is_pid_alive(Pid) when node(Pid) =:= node() ->  %% node(Pid) returns the node on which the pid is running
	is_process_alive(Pid);                      %% is_process_alive/1 is a BIF
is_pid_alive(Pid) ->
	lists:member(node(Pid), nodes()) andalso    %% check that Pid lives in one of the connected nodes...
	(rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true). %% and if so, do rpc call to that node
																	 %% check if proc is alive.
			
delete(Pid) ->
  %% 
  case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
		[#key_to_pid{} = Record] ->
			mnesia:dirty_delete_object(Record);
		_ ->
		    ok
  end.





