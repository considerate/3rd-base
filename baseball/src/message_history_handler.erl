-module (message_history_handler).
-export ([init/2]).
-define(MAX_MESSAGES,20).

init(Req, Opts) ->
	%{ok, _Tokendata} = auth_ball:authenticate(Req),
	%Thread = cowboy_req:binding(threadid, Req),
	%QS = cowboy_req:parse_qs(Req),
	%FromTo = {proplists:get_value(<<"after">>,QS),proplists:get_value(<<"before">>,QS)},
	%case FromTo of
		%Get all messages in thread
	%	{undefined,undefined} -> 
	%		StartKey = [Thread],
	%		EndKey = [Thread,{[]}];
		%Get all messages after StartKey
	%	{After,undefined} -> 
	%		StartKey = [Thread, After],
	%		EndKey = [Thread,{[]}];%{[]} is larger then any number in couchdb
		%Get MAX_MESSAGES messages before After and return link to get MAX_MESSAGES more if any
	%	{undefined,Before} ->
	%		
	%	{After,Before} ->
	%		io:format("Error: Supplying after and before not implemented for Threads path.")
		%Get (MAX_MESSAGES?) messages before After and return link fore more if any before Before
		
	%end,
	%EndKey = [Thread,{[]}],
	%JSONData = db_utils:query("/_design/Messages/_view/message_history", StartKey, EndKey),
	%BodyText = jiffy:encode(JSONData),
	%Headers = [{<<"Content-Type">>,<<"application/json">>}],
	%Response = cowboy_req:reply(200,
	%	Headers,
	%	BodyText,
	%	Req
	%),
	%{ok, Response, Opts}
	1.



get_row_value({Obj}) -> 
	Value = proplists:get_value(<<"value">>, Obj), 
	Value.
