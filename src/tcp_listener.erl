-module(tcp_listener).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).
-export([accept_func/1]).

-define(SERVER, ?MODULE). 
-define(LOGIC_MODULE, tcp_fsm).

-record(state, {
		  listener,       %% Listening socket
		  module          %% FSM handling module
		 }).

start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([Port]) ->
	Options = [{packet, raw}, {active, once}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Options) of
		{ok, LSocket} ->
			%% Create first accepting process
			spawn_link(?MODULE, accept_func, [LSocket]),
			{ok, #state{listener = LSocket, module   = ?LOGIC_MODULE}};
		{error, Reason} ->
			error_logger:error_msg("TCP Listener error start: ~p~n", [Reason]),
			{stop, Reason}
   end.


handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{listener = LSocket} = _State) ->
	gen_tcp:close(LSocket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

accept_func(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	error_logger:info_msg("Accept connection: ~p.\n", [Socket]),
	{ok, Pid} = tcp_client_sup:start_child(),
	ok = gen_tcp:controlling_process(Socket, Pid),	
	tcp_fsm:set_socket(Pid, Socket),
	accept_func(LSocket).
