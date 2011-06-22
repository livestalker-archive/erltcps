-module(tcp_server_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5). %% Max restarts
-define(MAX_TIME, 60).   %% Seconds between restarts

start_link(Port) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
	Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
	
	Spec = [
			{tcp_listener,                       %% Id
			 {tcp_listener, start_link, [Port]}, %% StartFunc = {M, F, A}
			 permanent,                          %% Permanent - child process is always restarted.
			 2000,                               %% Defines how a child process should be terminated. 2000 - timeout befor terminated.
			 worker,                             %% Type of child (worker | supervisor).
			 [tcp_listener]                      %% Callback module, shuld be a list with one element.
			},
			{tcp_client_sup,
			{tcp_client_sup, start_link, []},
			permanent,
			infinity,
			supervisor,
			[tcp_client_sup]}],
	
	{ok, {Flags, Spec}}.
