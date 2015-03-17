-module(baseball_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
	%Change the configuration of baseball by changing the tupple bellow
    Dispatch = cowboy_router:compile([
        {'_', [
        	{"/", hello_handler, []},
            {"/threads/:threadid/messages", message_history_handler, []},
        	{"/users/me/threads", my_threads_handler,[]},
            {"/threads/:threadid", thread_handler,[]}
        ]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8081}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	baseball_sup:start_link().
	

stop(_State) ->
	ok.
