-module(erltcps).
-behaviour(application).
-export([start/2, stop/1]).

-define(PORT, 9000).

start(_StartType, _StartArgs) ->
	case tcp_server_sup:start_link(?PORT) of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
				end.

stop(_State) ->
	ok.
