-module (db_utils).
-export ([query/1, query/2, query/3, connect_to_db/0,put_to_db/1,run_test_put/0,store_message/4,get_row_value/1,fetch/1]).
-define(BASE_ADDRESS,"http://localhost:5984/baseball").

connect_to_db() ->
	%initiate the http recuests thingy
	%TODO: ADD CODE TO START COUCH DB IF NOT STARTED
	_ = inets:start().

fetch(Id) when is_binary(Id) ->
	fetch(binary_to_list(Id));
fetch(Id) when is_list(Id)->
	io:format("~p", [Id]),
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ "/" ++ Id),
	jiffy:decode(Body).

query(Query) ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ Query),
	{Data} = jiffy:decode(Body),
	{_, Rows} = proplists:lookup(<<"rows">>, Data),
	{[{<<"rows">>,lists:map(fun get_row_value/1, Rows)}]}.

%queies cdb with a base quary and a list of supplied query parameters.
%Key must be a string
%Param must be parsable by jiffy
query(QueryBase,[{Key0,Param0}|T]) ->
	
	Query0 = "?" ++ Key0 ++ "=" ++ binary_to_list(jiffy:encode(Param0)),
	QueryN = lists:foldl(
	fun({KeyK,ParamK},QueryK) ->
		QueryK ++ "&" ++ KeyK ++ "=" ++ binary_to_list(jiffy:encode(ParamK))
	end,Query0,T),
	query(QueryBase ++ QueryN);
	
query(Query,Key) ->
	query(Query,[{"key",Key}]).
	
query(Query,StartKey,EndKey) ->
	Start = binary_to_list(jiffy:encode(StartKey)),
	End = binary_to_list(jiffy:encode(EndKey)),
	query(Query, [{"startkey",Start},{"endkey",End}]).
	
store_message(Message,Group,Sender,{time,Hour,Minute,Second}) ->
	put_to_db({[
		{<<"type">>, "message"},
		{<<"body">>,Message},
		{<<"group">>,Group},
		{<<"from">>,Sender},
		{<<"time">>,[Hour,Minute,Second]}
	]}).

%get_messages(Group,{time,StartHour,StartMinut,StartSecond},{time,EndHour,EndMinut,EndSecond}) ->
%	query("/_design/Messages/_view/message_history?startkey =\"">>,jiffy:encode([Group,[StartHour,StartMinut,StartSecond]]) ++ "\"&endkey=\""++ "[" ++ Group ++ ",[" ++ EndHour ++ "," ++ EndMinut ++ "," ++ EndSecond ++ "]" ++ "]" ++ "]" ++ "\"").

put_to_db(StuffsToAdd) -> 
	% Specifying options for http request to db
	Method = post,
	Url = ?BASE_ADDRESS,	
	Headers = [],
	Content_type = "application/json",
	Body = jiffy:encode(StuffsToAdd),
	Request = {Url,Headers,Content_type,Body},
	HTTPOptions = [],
	Options = [],

	%Making request to db
	httpc:request(Method,Request,HTTPOptions,Options).

run_test_put() -> put_to_db({[
		{<<"Subject">>,<<"I like Plankton">>},
		{<<"Author">>,<<"Rusty">>},
		{<<"PostedDate">>,<<"2006-08-15T17:30:12-04:00">>},
		{<<"Tags">>,[<<"plankton">>,<<"baseball">>,<<"decisions">>]},
		{<<"Body">>,<<"I decided today that I don't like baseball. I like plankton.">>}
	]}).

get_row_value({Obj}) -> 
	Value = proplists:get_value(<<"value">>, Obj), 
	Value.

%The above erlang datastructure corresponds to the following json
%"{ 
%
%			  "Subject":"I like Plankton",
%			  "Author":"Rusty",
%			  "PostedDate":"2006-08-15T17:30:12-04:00",
%			  "Tags":["plankton", "baseball", "decisions"],
%			  "Body":"I decided today that I don't like baseball. I like plankton."
%			}"
%	).
