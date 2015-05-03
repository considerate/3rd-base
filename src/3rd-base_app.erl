-module ('3rd-base_app').
-behaviour(application).
-export([start/2,stop/1]).
trim_whitespace(Input) -> re:replace(Input, "\\s+", "", [global]).
start(_Type, _Args) ->
    fubar:start(),
    apns:connect(apns),
    {ok, Path} = application:get_env(gcm, secret_file),
    {ok, File} = file:open(Path, [read]),
    {ok, Secret} = file:read_line(File),
    gcm:start_link(g_cloud, trim_whitespace(Secret)),
    '3rd-base_sup':start_link().

stop(_State) ->
    ok.
