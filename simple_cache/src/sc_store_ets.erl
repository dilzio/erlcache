%% Copyright
-module(sc_store_ets).
-author("dilzio").

%% API
-export([init/0,
         insert/2,
         delete/1,
         lookup/1]).

-define(TABLE_ID, ?MODULE).
-record(key_to_pid, {key, pid}).

init() ->
  mnesia:start(),
  mnesia:create_table(key_to_pid, 	
					  [{index, [pid]},  %% declare index on pid column
					   {attributes, record_info(fields, key_to_pid)}
					  ]).

insert(Key, Pid) ->
  	mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).

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





