-module(ti_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).
    
%% init returns a timeout of 0 which causes OTP to immediately call handle_info({timeout, State})
%% which where the rest of the TCP setup is handled. This is done so that init/1 doesn't block.
init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};

%% called when socket is closed...this proc's job is done so close
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

%% called by framework on timeout, accept connection then tell supervisor to start a new child to listen for the
%% next process
%% accept will block until next incoming connection which is why we need to decouple it from init
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    ti_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
handle_data(Socket, RawData, State) ->
    try
        {Function, RawArgList} = lists:splitwith(fun(C)->C =/= $[ end, RawData),
        {ok, Toks, _Line} = erl_scan:string(RawArgList ++ ".", 1),
        {ok, Args} = erl_parse:parse_term(Toks),
        Result = apply(simple_cache, list_to_atom(Function), Args),
        gen_tcp:send(Socket, io_lib:fwrite("OK:~p ~n", [Result]))
    catch
        _Class:Err ->
            gen_tcp:send(Socket, io_lib:fwrite("ERROR:~p. ~n", [Err]))
    end,
    State.
