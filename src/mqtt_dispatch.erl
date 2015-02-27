-module(mqtt_dispatch).
-behavior(mqtt_protocol).

%%
%% Exports
%%
-export([mnesia/1, get_address/0, get_address/1, set_address/1, apply_setting/1]).

%%
%% mqtt_protocol callbacks
%%
-export([init/1, handle_message/2, handle_event/2, terminate/2]).

-include("deps/fubar/include/fubar.hrl").
-include("deps/fubar/include/mqtt.hrl").


%% @doc Mnesia table manipulations.
mnesia(Action) -> mqtt_server:mnesia(Action).

apply_setting(Setting) -> mqtt_server:apply_setting(Setting).


%% @doc Get address of this node.
-spec get_address() -> {ok, string()} | {error, not_found}.
get_address() ->
	get_address(node()).

%% @doc Get address of a node.
-spec get_address(node()) -> {ok, string()} | {error, not_found}.
get_address(Node) -> mqtt_server:get_address(Node).

%% @doc Set address of this node.
-spec set_address(string()) -> ok.
set_address(Address) -> mqtt_server:set_address(Address).

-define(KEEPALIVE_MULTIPLIER, 2000).

-define (CONTEXT, mqtt_server).

%% mqtt_protocol context
-record(?CONTEXT, {
		client_id :: binary(),
		auth :: module(),
		session :: pid(),
		valid_keep_alive = {1800, 3600} :: {MinSec :: integer(), MaxSec :: integer()},
		when_overloaded = drop :: drop | accept,
		overloaded=false,
		load_balancing = none,
		timeout = 10000 :: timeout(),
		timestamp :: timestamp()
}).

-type context() :: #?CONTEXT{}.

-spec init(params()) -> {noreply, context(), timeout()}.
init(Params) ->
	mqtt_server:init(Params).

-spec handle_message(mqtt_message(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term()}.
handle_message(Message=#mqtt_publish{topic= <<"newchat">>}, Context) ->
	io:format("New Chat message ~p", [Message#mqtt_publish.payload]),
	mqtt_server:handle_message(Message,Context);
handle_message(Message,Context) -> 
	mqtt_server:handle_message(Message,Context).

-spec handle_event(Event :: term(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term(), context()}.
handle_event(Event, Context) ->
	mqtt_server:handle_event(Event,Context).

-spec terminate(Reason :: term(), context()) -> Reason :: term().
terminate(Reason, Context) -> 
	mqtt_server:terminate(Reason, Context).

%%
%% Unit Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
