-module(couch_base_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
        	{"/", hello_handler, []}
        ]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8081}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	couch_base_sup:start_link().
	

stop(_State) ->
	ok.
