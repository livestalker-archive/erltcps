{application, erltcps, [
			  		   {description, "Erlang TCP Server"},
					   {vsn, "1.0"},
					   {modules,      [erltcps, 
					   				  tcp_server_sup, 
									  tcp_listener, 
									  tcp_client_sup,
									  tcp_fsm]},
					   {registered,   []},
					   {applications, [kernel, stdlib]},
			  		   {mod, {erltcps, []}},
					   {env, []}
			  		   ]}.