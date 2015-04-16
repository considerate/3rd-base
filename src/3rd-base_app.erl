-module ('3rd-base_app').
-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _Args) ->
    fubar:start(),
    '3rd-base_sup':start_link().

stop(_State) ->
    ok.
