-module(get_groups_handler).
-export([init/2]).

-define (SECRET, <<"This is a very secret secret, do not tell anyone.">>).

init(Req,Opts) ->
	Headers = cowboy_req:headers(Req),
    {_,Auth}=proplists:lookup(<<"authorization">>,Headers),
    <<"Bearer ", Token/binary>> = Auth,
    case ejwt:parse_jwt(Token, ?SECRET) of
    	invalid -> cowboy_req:reply(403,Req);
    	expired -> cowboy_req:reply(401,Req);
		{Data} -> 
			Uid = proplists:get_value(<<"id">>, Data),
			{Groups} = db_utils:query("/_design/user_group/_view/groups", Uid),
			{_, Rows} = proplists:lookup(<<"rows">>, Groups),
			JSONData = {[{<<"rows">>,lists:map(fun db_utils:get_row_value/1, Rows)}]},
			BodyText = jiffy:encode(JSONData),
			ResponseHeaders = [{<<"Content-Type">>,<<"application/json">>}],
			Response = cowboy_req:reply(200,
				ResponseHeaders,
				BodyText,
				Req
			),
			{ok, Response, Opts}
	end.
