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

-record(mqtt_presence, {user,clients,last_status}).

%% @doc Mnesia table manipulations.
mnesia(Action) ->
    mqtt_server:mnesia(Action),
    mnesia:create_table(mqtt_presence, [{ram_copies, [node()]}, {attributes, record_info(fields,mqtt_presence)}, {type,set}]).

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

-define(ID_REGEX, "([^/]+)").
-define(THREAD_REGEX, snd(re:compile("thread/" ++ ?ID_REGEX))).
-define(ONLINE_REGEX, snd(re:compile("online/" ++ ?ID_REGEX))).

snd({_X,Y}) -> Y.

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
           timestamp :: timestamp(),
           data = [] :: [proplists:property()]
          }).


-type context() :: #?CONTEXT{}.

-spec init(params()) -> {noreply, context(), timeout()}.
init(Params) ->
    random:seed(erlang:now()),
    mqtt_server:init(Params ++ [
                                {data, [
                                        {objectid, objectid:objectid()}
                                       ]}
                               ]).


-spec is_empty(sets:set()) -> true | false.
is_empty(Set) ->
    sets:is_subset(Set,sets:new()).

persist_message(Message,Context,Session,ThreadId,Body,Data) ->
    {_, ObjectId} = proplists:lookup(objectid, Data),
    {Id, NextId} = ObjectId,
    {ok,State} = gen_server:call(Session,state),
    Username = mqtt_session:username(State),
    db_utils:store_message(Id, Body, ThreadId, Username),
    NewData = [{objectid, NextId()}|proplists:delete(objectid,Data)],
    io:format("Updated data ~p ~n",[NewData]),
    mqtt_server:handle_message(Message,Context#?CONTEXT{data=NewData}).

online_status(Message,Context,Status,UserId,ClientId) ->
    io:format("online/~p",[UserId]),
    case Status of
        <<"offline">> ->
           case logoff(UserId,ClientId) of
               {atomic, Clients} ->
                   case is_empty(Clients) of
                       true ->
                            mqtt_server:handle_message(Message,Context);
                       false ->
                            {noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout}
                   end;
               _ ->
                    {noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout}
           end;
        _ ->
            case update_status(Status,UserId,ClientId) of
                {atomic, {Status,_Clients}} ->
                    {noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout}
                    ;
                {atomic, {_PrevStatus,_Clients}} ->
                    mqtt_server:handle_message(Message,Context);
                _ ->
                    {noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout}
            end
    end.

update_status(Status,UserId,ClientId) ->
    Transaction = fun() ->
        case mnesia:wread({mqtt_presence,UserId}) of
            [P=#mqtt_presence{clients=Clients,last_status=PrevStatus}] ->
                NewClients = sets:add_element(ClientId,Clients),
                mnesia:write(P#mqtt_presence{clients=NewClients,last_status=Status}),
                {PrevStatus,NewClients}
                ;
            _DoesNotExist ->
              Clients = sets:new(),
              NewClients = sets:add_element(ClientId, Clients),
              mnesia:write(#mqtt_presence{user=UserId,clients=NewClients,last_status=Status}),
              {none,NewClients}
        end
    end,
    mnesia:transaction(Transaction).

logoff(UserId,ClientId) ->
    Transaction = fun() ->
        case mnesia:wread({mqtt_presence,UserId}) of
            [P=#mqtt_presence{clients=Clients}] ->
                NewClients = sets:del_element(ClientId,Clients),
                mnesia:write(P#mqtt_presence{clients=NewClients}),
                NewClients
                ;
            _DoesNotExist ->
                mnesia:abort("No such presence object")
        end
    end,
    mnesia:transaction(Transaction).




-spec handle_message(mqtt_message(), context()) ->
    {reply, mqtt_message(), context(), timeout()} |
    {noreply, context(), timeout()} |
    {stop, Reason :: term()}.
handle_message(
  Message=#mqtt_publish{topic = Topic, payload=Payload},
  Context=#?CONTEXT{session=Session, data=Data,client_id=ClientId}
 ) when is_pid(Session) ->
    RegOpts = [{capture,all_but_first,binary}],
    case re:run(Topic, ?THREAD_REGEX, RegOpts) of
        {match, [ThreadId]} ->
            {JSON} = jiffy:decode(Payload),
            Body = proplists:get_value(<<"body">>, JSON),
            persist_message(Message,Context,Session,ThreadId,Body,Data);
        _ ->
            case re:run(Topic, ?ONLINE_REGEX,RegOpts) of
                {match, [UserId]} ->
                    {JSON} = jiffy:decode(Payload),
                    Status = proplists:get_value(<<"status">>, JSON),
                    online_status(Message,Context,Status,UserId,ClientId);
                _ ->
                    mqtt_server:handle_message(Message,Context)
            end
    end;

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
