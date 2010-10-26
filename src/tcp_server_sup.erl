%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov alexey@livestalker.net
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @doc
%%% TCP Sever Supervisor
%%% @end
%%% Created :  5 Oct 2010 by Alexey Grebenshchikov
%%%-------------------------------------------------------------------
-module(tcp_server_sup).
-author('alexeylivestalker.net').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5). %% Max restarts
-define(MAX_TIME, 60).   %% Seconds between restarts

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(integer) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
	%% supervisor flags
	Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
	
	%% specification of child processes
	Spec = [
			{tcp_listener, 
			 {tcp_listener, start_link, [Port]}, 
			 permanent,
			 2000,
			 worker,
			 [tcp_listener]
			},
			{tcp_client_sup,
			{tcp_client_sup, start_link, []},
			permanent,
			infinity,
			supervisor,
			[tcp_client_sup]}],
	
	{ok, {Flags, Spec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
