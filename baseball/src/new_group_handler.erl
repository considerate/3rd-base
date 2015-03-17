-module(new_grop_handler).
-export([init/2]).

-init(Req,Opts) ->
	Headers = cowboy_req:headers(Req),
	
