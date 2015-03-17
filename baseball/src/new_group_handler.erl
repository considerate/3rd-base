-module(new_grop_handler).
-export([init/2]).

-define (SECRET, <<"This is a very secret secret, do not tell anyone.">>).

-init(Req,Opts) ->
	Headers = cowboy_req:headers(Req),
    {_,Auth}=proplists:lookup(<<"Athorization">>,Headers),
    "Bearer " ++ Token = Auth,
    case jwt:decode(Token, ?SECRET) of
		{ok,Data} -> 
			Username = proplists:get_value(<<"id">>, Data),
				
		Error ->
			Error
	end.
	
