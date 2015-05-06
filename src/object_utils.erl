-module(object_utils).
-export([thread_data/1,valid_thread_name/1,add_fields_safely_from_json_source/3]).

thread_data({ThreadData}) ->
    BaseObj = [
        {id, proplists:get_value(<<"_id">>,ThreadData)},
        {users, proplists:get_value(<<"users">>, ThreadData)},
        {creator, proplists:get_value(<<"creator">>,ThreadData)}
    ],
    add_fields_safely_from_json_source({BaseObj},[<<"name">>],{ThreadData}).
    
valid_thread_name(Name) ->
    Name =/= <<"">> andalso is_bitstring(Name).
    
add_fields_safely_from_json_source({BaseObj},FieldsToAdd,{SourceJSON}) ->
	RetObj = lists:foldr(
        fun(FieldName,AccObj) ->
     
            case proplists:get_value(FieldName,SourceJSON) of
                undefined ->
                    AccObj;
                Value ->
                    [{FieldName, Value}|AccObj]
            end
        end, 
        BaseObj,
        FieldsToAdd
    ),
    {RetObj}.

