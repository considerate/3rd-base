-module(thread_handler).
-export([init/2]).

init(Req,Opts) ->
    case auth_ball:authenticate(Req) of
		{ok, Data} -> 
			Thread = cowboy_req:binding(threadid, Req),
			Uid = proplists:get_value(<<"id">>, Data),
			JSONData = db_utils:fetch(Thread),
			
			% TODO: Add logic for not member of thread
			BodyText = jiffy:encode(JSONData),
			ResponseHeaders = [{<<"Content-Type">>,<<"application/json">>}],
			Response = cowboy_req:reply(200,
				ResponseHeaders,
				BodyText,
				Req
			),
			{ok, Response, Opts};
		Error ->
			io:format("ERROR: ~p", [Error]),
			cowboy_req:reply(401,Req)
	end.