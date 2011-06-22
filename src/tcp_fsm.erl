-module(tcp_fsm).
-behaviour(gen_fsm).
-export([start_link/0, set_socket/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([wait_for_socket/2, wait_for_data/2]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 600000).

-record(state, 
		{
		  socket,  %% client socket
		  addr     %% ip client
		}).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

init([]) ->
	{ok, wait_for_socket, #state{}}.

wait_for_socket({socket_ready, Socket}, StateData) when is_port(Socket) ->
	inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}, {keepalive, true}]),	
	{ok, {Address, _Port}} = inet:peername(Socket),
    error_logger:info_msg("IP: ~p~n", [Address]),
    {next_state, wait_for_data, StateData#state{socket=Socket, addr=Address}, ?TIMEOUT};

wait_for_socket(Other, StateData) ->
    error_logger:error_msg("State: wait_for_socket. Unexpected message: ~p~n", [Other]),
    {next_state, wait_for_socket, StateData}.

wait_for_data({data, Bin}, #state{socket=Socket} = StateData) ->
    %% echo to client
	ok = gen_tcp:send(Socket, Bin),
	inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}, {keepalive, true}]),	
    {next_state, wait_for_data, StateData, ?TIMEOUT};

wait_for_data(timeout, #state{addr=Address} = StateData) ->
    error_logger:info_msg("~p Client connection timeout - closing.~n", [Address]),
    {stop, normal, StateData};

wait_for_data(Other, StateData) ->
    error_logger:error_msg("State: wait_for_data. Unexpected message: ~p~n", [Other]),
    {next_state, wait_for_data, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	Reply = ok,
	{reply, Reply, StateName, StateData}.

handle_info({tcp, _Socket, Bin}, StateName, StateData) ->
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, _Socket}, _StateName, #state{addr=Address} = StateData) ->
    error_logger:info_msg("~p Client disconnected.~n", [Address]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
