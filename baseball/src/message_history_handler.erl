-module (message_history_handler).
-export ([init/2]).

init(Req, Opts) ->
	{ok, _Tokendata} = auth_ball:authenticate(Req),
	Thread = cowboy_req:binding(threadid, Req),
	QS = cowboy_req:parse_qs(Req),
	After = proplists:get_value(<<"after">>,QS),
	case After of
		undefined -> 
			StartKey = [Thread];
		_ -> 
			StartKey = [Thread, After]
	end,
	EndKey = [Thread,{[]}],
	JSONData = db_utils:query("/_design/Messages/_view/message_history", StartKey, EndKey),
	BodyText = jiffy:encode(JSONData),
	Headers = [{<<"Content-Type">>,<<"application/json">>}],
	Response = cowboy_req:reply(200,
		Headers,
		BodyText,
		Req
	),
	{ok, Response, Opts}.



get_row_value({Obj}) -> 
	Value = proplists:get_value(<<"value">>, Obj), 
	Value.
