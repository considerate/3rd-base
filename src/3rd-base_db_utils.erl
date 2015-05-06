-module ('3rd-base_db_utils').
-export ([
    query/1,
    query/2,
    query/3,
    put_to_db/2,
    put_to_db/1,
    store_message/4,
    add_thread/5,
    get_row_value/1,
    fetch/1,
    document_exists/3
]).
-define(BASE_ADDRESS,"http://localhost:5984/baseball").

fetch(Id) when is_binary(Id) ->
    fetch(binary_to_list(Id));
fetch(Id) when is_list(Id)->
    {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ "/" ++ Id),
    case StatusCode of
    	200 ->
    		{ok,jiffy:decode(Body)};
    	Err ->
    		{error,Err}
    end.

query(Query)->
    query(Query,{opts,[]}).

query(Query, {opts,Opts}) when is_binary(Query) ->
    NewQuery = binary_to_list(Query),
    query(NewQuery,{opts,Opts});
query(Query,{opts,Opts}) when is_list(Query) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ Query),
    {Data} = jiffy:decode(Body),
    Rows = proplists:get_value(<<"rows">>, Data),
    NewRows = case proplists:get_bool(reverse,Opts) of
        false ->
            Rows;
        true ->
            lists:reverse(Rows)
    end,
    {[{<<"rows">>,lists:map(fun get_row_value/1, NewRows)}]};
query(QueryBase,[{Key,Param}|Rest]) ->
    query(QueryBase,[{Key,Param}|Rest], {opts,[]});
query(Query,Key) ->
    query(Query,[{"key",Key}]).

query(BasePath, [{Key,Param}|Rest], {opts,Opts}) ->
    AllParams = [{Key,Param}|Rest],
    QS = binary_to_list(web_utils:querystring(AllParams)),
    query(BasePath ++ QS, {opts, Opts});
query(Query,StartKey,EndKey) ->
    Start = binary_to_list(jiffy:encode(StartKey)),
    End = binary_to_list(jiffy:encode(EndKey)),
    query(Query, [{"startkey",Start},{"endkey",End}]).

bin_to_hex(Bin) when is_binary(Bin) ->
    << <<Y>> || <<X:4>> <= Bin, Y <- integer_to_list(X,16) >>.

store_message(BinId, JSONMessage, Thread, Sender) ->
    Id = bin_to_hex(BinId),
    BaseMessage = {[
        {<<"_id">>,Id},
        {<<"type">>, <<"message">>},
        {<<"thread">>,Thread},
        {<<"from">>,Sender}
    ]},
    Message = object_utils:add_fields_safely_from_json_source(BaseMessage,[<<"body">>,<<"image">>],JSONMessage),
    put_to_db(Id,Message).
    
    

add_thread(Id,Users,Name,Creator,Private) ->
    BaseOutput = [
                {<<"type">>,<<"thread">>},
                {<<"_id">>,Id},
                {<<"users">>,Users},
                {<<"creator">>,Creator},
                {<<"private">>, Private}
                ],
    Output = case object_utils:valid_thread_name(Name) of
        true ->
            {[{<<"name">>,Name}|BaseOutput]};
        false -> 
            {BaseOutput}
    end,
    put_to_db(Id,Output).

put_to_db({JSONData}) ->
    put_to_db(proplists:get_value(<<"_id">>,JSONData),{JSONData}).

put_to_db(Id,JSONData) ->
    % Specifying options for http request to db
    Method = put,
    Url = ?BASE_ADDRESS ++ "/" ++ binary_to_list(Id),
    Headers = [],
    Content_type = "application/json",
    Body = jiffy:encode(JSONData),
    Request = {Url,Headers,Content_type,Body},
    HTTPOptions = [],
    Options = [],

    %Making request to db
    httpc:request(Method,Request,HTTPOptions,Options).

get_row_value({Obj}) ->
    Value = proplists:get_value(<<"value">>, Obj),
    Value.
    
%Checks if a document  provided in DocumentType exists in the db.
%Prepends the Document to state if it exists
document_exists(DocumentType,Req,State) when is_atom(DocumentType) ->
    ID = cowboy_req:binding(DocumentType, Req),
    case fetch(ID) of
        {ok,JSONData} ->
            {true,Req,[{document,JSONData}|State]};
        {error,_Err} ->
            {false,Req,State}
    end.
    
