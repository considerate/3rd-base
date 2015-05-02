-module('3rd-base_push').
-export([
    push_message/3
]).

-record(token, {type, token}).

-include_lib("apns/include/apns.hrl").

push_message_user(Text, Extra, #token{type=apns, token=Token}) ->
    apns:send_message(apns, #apns_msg{
      alert=erlang:binary_to_list(Text),
      extra=erlang:binary_to_list(jiffy:encode(Extra)),
      device_token=Token,
      sound="default"
    }),
    ok;
push_message_user(Text, Extra, #token{type=gcm, token=Token}) ->
    notimplemented;
push_message_user(_Text, _Extra, #token{type=Type}) ->
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

-spec push_message_user(any(), [binary()], [proplists:property()], binary()) -> ok | nopush.
push_message_user(_Context, Thread, Message, UserId) ->
    Text = jiffy:encode({Message}),
    ThreadId = proplists:get_value(id, Thread),
    Extra = {[{thread, ThreadId},
              {message, {Message}}]},
    UserData = user_data(UserId),
    case proplists:get_value(<<"pushTokens">>, UserData) of
        PushTokens when is_list(PushTokens) ->
            lager:debug("would push ~p to ~p via ~p", [Message, UserId, PushTokens]),
            lists:map(fun (TokenObj) ->
                          push_message_user(Thread, Message, token_record(TokenObj))
                      end,
                      PushTokens),
            ok;
        _ ->
            lager:debug("no push for ~p", [UserId]),
            nopush
    end.

-spec push_message_users(any(), [binary()], [proplists:property()], [binary()]) -> [ ok | nopush ].
push_message_users(Context, Thread, Message, [UserId|Users]) ->
    Result = push_message_user(Context, Thread, Message, UserId),
    [ Result | push_message_users(Context, Thread, Message, Users) ];
push_message_users(Context, _, _, []) ->
    [].

-spec push_message(any(), binary(), [proplists:property()]) -> ok.
push_message(Context, ThreadId, Message) ->
    {ok, {Thread}} = '3rd-base_db_utils':fetch(ThreadId),
    Users = proplists:get_value(<<"users">>, Thread),
    push_message_users(Context, Thread, Message, Users),
    ok.
