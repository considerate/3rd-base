-module(auth_ball).
-export([authenticate/1]).
-define (SECRET, <<"This is a very secret secret, do not tell anyone.">>).

authenticate(Req) ->
    Headers = cowboy_req:headers(Req),
    {_,Auth}=proplists:lookup(<<"authorization">>,Headers),
    <<"Bearer ", Token/binary>> = Auth,
    ejwt:parse_jwt(Token, ?SECRET).
