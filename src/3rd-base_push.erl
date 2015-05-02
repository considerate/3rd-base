-module('3rd-base_push').
-export([
    push_message/2
]).

-record(token, {type, token}).

-include_lib("apns/include/apns.hrl").

-spec is_online(binary()) -> online | offline.
is_online(UserId) ->
    case mnesia:dirty_read(mqtt_presence, UserId) of
        [{mqtt_presence, _, Clients, _}] ->
            case sets:size(Clients) > 0 of
                true ->
                    online;
                false ->
                    offline
            end;
        _DoesNotExist ->
            offline
    end.

push(Text, Extra, #token{type=apns, token=Token}) ->
    apns:send_message(apns, #apns_msg{
      alert=Text,
      extra=Extra,
      device_token=Token,
      sound="default"
    }),
    ok;
push(Text, Extra, #token{type=gcm, token=Token}) ->
    notimplemented;
push(_Text, _Extra, #token{type=Type}) ->
    lager:error("unknown token type ~p", [Type]).

token_record({TokenObj}) ->
    Type = proplists:get_value(<<"type">>, TokenObj),
    Token = proplists:get_value(<<"token">>, TokenObj),
    #token{type=erlang:binary_to_atom(Type, utf8),
           token=erlang:binary_to_list(Token)}.

user_data(UserId) ->
    {ok, Settings} = application:get_env(homebase, user_api),
    BasePath = proplists:get_value(base_path, Settings),
    URL = BasePath ++ "/users/" ++ binary_to_list(UserId),
    {ok, {_Status, _Headers, UserBody}} = httpc:request(URL),
    {UserData} = jiffy:decode(UserBody),
    UserData.

-spec push_message_user([binary()], [proplists:property()], binary()) -> ok | error.
push_message_user(Thread, Message, UserId) ->
    Text = make_nice_texts(Message),
    ThreadId = proplists:get_value(<<"_id">>, Thread),
    Extra = [{thread, ThreadId},
             {message, {Message}}],
    UserData = user_data(UserId),
    case proplists:get_value(<<"pushTokens">>, UserData) of
        PushTokens when is_list(PushTokens) ->
            lists:map(fun (TokenObj) ->
                          lager:debug("push ~p, ~p", [Text, Extra]),
                          push(Text, Extra, token_record(TokenObj))
                      end,
                      PushTokens),
            ok;
        _ ->
            lager:debug("no push for ~p", [UserId]),
            nopush
    end.

-spec push_message_users([binary()], [proplists:property()], [binary()]) -> [ ok | error | nopush ].
push_message_users(Thread, Message, [UserId|Users]) ->
    Result = case is_online(UserId) of
        offline ->
            push_message_user(Thread, Message, UserId);
        online ->
            nopush
    end,
    [ Result | push_message_users(Thread, Message, Users) ];
push_message_users(_, _, []) ->
    [].
    
make_nice_texts(Message) ->
    From = proplists:get_value(<<"from">>,Message),
    Text = proplists:get_value(<<"body">>,Message),
    io_lib:format("~s: ~s",[From, Text]).
    %% << From/binary, <<": ">>/binary, Text/binary >>.

-spec push_message(binary(), [proplists:property()]) -> ok | error.
push_message(ThreadId, Message) ->
    {ok, {Thread}} = '3rd-base_db_utils':fetch(ThreadId),
    Users = proplists:get_value(<<"users">>, Thread),
    push_message_users(Thread, Message, Users),
    ok.
