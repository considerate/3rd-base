-module (jwt_auth).

%%
%% Exports
%%
-export([verify/2]).

-define (SECRET, <<"This is a very secret secret, do not tell anyone.">>).


%% @doc Verify credential.
verify(Username, Token) ->
    case ejwt:parse_jwt(Token, ?SECRET) of
    	invalid -> {error, forbidden};
    	expired -> {error, forbidden};
		{Data} -> 
			case proplists:get_value(<<"id">>, Data) of
				Username ->
					{ok, Data};
				_ -> 
					{error, forbidden}
			end;
		Error ->
			{error, Error}
	end.

%%
%% Unit Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
