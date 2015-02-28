-module (jwt_auth).

%%
%% Exports
%%
-export([verify/2]).


-define(MNESIA_TIMEOUT, 10000).
-define (SECRET, <<"This is a very secret secret, do not tell anyone.">>).


%% @doc Verify credential.
verify(Username, Token) ->
	case jwt:decode(Token, ?SECRET) of
		{ok,Data} -> 
			case proplists:get_value(<<"id">>, Data) of
				Username ->
					{ok, Data};
				_ -> 
					{error, forbidden}
			end;
		Error ->
			Error
	end.

%%
%% Unit Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
