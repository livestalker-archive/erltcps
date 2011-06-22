-module(tcp_client_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5). %% Max restarts
-define(MAX_TIME, 60).   %% Seconds between restarts

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
	supervisor:start_child(tcp_client_sup, []).

init([]) ->
	%% supervisr flags
	Flags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
	%% specification of child processes
	Spec = [
			{undefined, 
			 {tcp_fsm, start_link, []}, 
			 temporary,
			 2000,
			 worker,
			 [tcp_fsm]
			}],
	{ok, {Flags, Spec}}.
