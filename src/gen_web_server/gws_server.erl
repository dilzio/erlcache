-module(gws_server).

-behaviour(gen_server).

%% API exports
-export([start_link/3]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%lsock=listening socket, socket=dedicated socket, request_line, headers, body, content_remaining
% are all for handling http protocol, callback is the name of the behaviour implementation module,
% user_data is data to be passed to the callback, parent is the PID of the gws_connection_sup proc
-record(state, {lsock, socket, request_line, headers = [], body = <<>>, content_remaining=0, callback, user_data, parent}).


%% API impl
% args are callback module, listening socket and user args. These are passed to the start_link
% self() refers to the pid of the caller who is assumed to be the supervisor process
% which is starting the gws_server
start_link(Callback, LSock, UserArgs) ->
    gen_server:start_link(?MODULE, [Callback, LSock, UserArgs, self()], []).


%% gen_server callback impl

% called by gen_server container.  Gets a UserData data structure from the callback, then
% sets up the state record. 0 return value means process will timeout immediately which
% will call handle_info(timeout) on another thread to finish initialization.
init([Callback, LSock, UserArgs, Parent]) ->
    {ok, UserData} = Callback:init(UserArgs),
    State = #state{lsock = LSock, callback = Callback, user_data = UserData, parent = Parent},
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

% becasue the listening socket was configured with {packet, http_bin} the incoming messages
% will be parsed as per the http protocol and per the erlang:decode_packet/3 specs
% serveral calls to handle_info will be made for each client request, each with different parts
% of the request

% handle request line from socket: {http_request, HttpMethod, HttpUri, HttpVersion}
handle_info({http, _Sock, {http_request, _, _, _}=Request}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, State#state{request_line= Request}};

% handle a header line: {http_request, HttpMethod, HttpUri, HttpVersion}
% header function will parse out any headers it cares about and stick the values in the State, 
% and return an updated copy of the state.
handle_info({http, _Sock, {http_header, _, Name, _, Value}}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, header(Name, Value, State)};

% handle the end of header line: {http, _Socket, http_eoh} 
% when the content_remaining is 0, i.e, when all content of body has been read off the socket
handle_info({http, _Sock, http_eoh}, #state{content_remaining = 0} = State) ->
    {stop, normal, handle_http_request(State)};

% handle the end of header line when a nonempty body is received
% the automatic http parsing is switched off by setting {packet, raw} because the data
% can have any form. Subsequent callbacks from the socket will match the following clause
handle_info({http, _Sock, http_eoh}, State) ->
    inet:setopts(State#state.socket, [{active, once},{packet, raw}]),
    {noreply, State};

% Once packet processing has been set back to raw, all calls from the socket will come in
% as {tcp, Socket, Data}.
% the content_remaining value in the state is decremented by the size of the of the Data being passed in
handle_info({tcp, _Sock, Data}, State) when is_binary(Data) ->
    ContentRem = State#state.content_remaining - byte_size(Data),
    Body       = list_to_binary([State#state.body, Data]),
    NewState   = State#state{body = Body, content_remaining = ContentRem},

    if ContentRem > 0 ->
            inet:setopts(State#state.socket, [{active, once}]),
            {noreply, NewState};
            true ->
                {stop, normal, handle_http_request(NewState)}
    end;

handle_info({tcp_closed, _Sock}, State)->
    {stop, normal, State};
% gets called when process timesout (timeout set to 0 in init/1).
% accepts connection on listening socket calls the gws_connection_sup superviser to start a new child
% passing in the PID of the supervisor process to use.
% dedicated socket is set to active,once for flow control and the socket reference is added
% to the record
handle_info(timeout, #state{lsock = LSock, parent = Parent} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    gws_connection_sup:start_child(Parent),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

% stores Value from Content-Length header for user later in retrieving the body
header('Content-Length' = Name, Value, State) ->
    ContentLength = list_to_integer(binary_to_list(Value)),
    State#state{content_remaining = ContentLength, headers=[{Name, Value} | State#state.headers]};

% handles expect header by immediately sending a message to the client to continue sending
% the body of the request
header(<<"Expect">> = Name, <<"100-continue">> = Value, State) ->
    gen_tcp:send(State#state.socket, gen_web_server:http_reply(100)),
    State#state{headers = [{Name, Value} | State#state.headers]};

% handle any remaining headers by adding to headers collection
header(Name, Value, State) ->
    State#state{headers = [{Name, Value} | State#state.headers]}.

% finally got the request put together, now tease out all fields from the State (including
% request Method from the Request
handle_http_request(#state{callback = Callback,
                           request_line = Request,
                           headers      = Headers,
                           body         = Body,
                           user_data    = UserData} = State) ->
    {http_request, Method, _, _} = Request,
    Reply = dispatch(Method, Request, Headers, Body, Callback, UserData),
    gen_tcp:send(State#state.socket, Reply),
    State.

% call the appropriate callback function
dispatch('GET', Request, Headers, _Body, Callback, UserData) ->
    Callback:get(Request, Headers, UserData);
dispatch('DELETE', Request, Headers, _Body, Callback, UserData) ->
    Callback:delete(Request,Headers, UserData);
dispatch('HEAD', Request, Headers, _Body, Callback, UserData) ->
    Callback:head(Request,Headers, UserData);
dispatch('POST', Request, Headers, Body, Callback, UserData) ->
    Callback:post(Request,Headers, Body, UserData);
dispatch('PUT', Request, Headers, Body, Callback, UserData) ->
    Callback:put(Request,Headers, Body, UserData);
dispatch('TRACE', Request, Headers, Body, Callback, UserData) ->
    Callback:trace(Request,Headers, Body, UserData);
dispatch('OPTIONS', Request, Headers, Body, Callback, UserData) ->
    Callback:options(Request,Headers, Body, UserData);
dispatch(_Other, Request, Headers, Body, Callback, UserData) ->
    Callback:other_methods(Request,Headers, Body, UserData).

