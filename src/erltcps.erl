%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov alexey@livestalker.net
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @doc
%%% TCP Listener
%%% @end
%%% Created :  5 Oct 2010 by Alexey Grebenshchikov
%%%-------------------------------------------------------------------
-module(erltcps).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(PORT, 9000).     %% Default port

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	Port = get_app_env(listen_port, ?PORT),
	case tcp_server_sup:start_link(Port) of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
				end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_app_env(Key, Default) ->
    case application:get_env(Key) of
		{ok, Val} -> Val;
		undefined -> Default
    end.
