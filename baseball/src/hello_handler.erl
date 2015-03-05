-module(hello_handler).
-export([init/2]).

init(Req, Opts) ->
    Response = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello Erlang!">>,
        Req),
    {ok, Response, Opts}.
