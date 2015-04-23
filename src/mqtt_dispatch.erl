-module(mqtt_dispatch).
-behavior(mqtt_protocol).

%%
%% Exports
%%
-export([mnesia/1, get_address/0, get_address/1, set_address/1, apply_setting/1]).

%%
%% mqtt_protocol callbacks
%%
-export([init/1, handle_message/2, handle_event/2, handle_will_message/2, terminate/2]).

-include_lib("fubar/include/fubar.hrl").
-include_lib("fubar/include/mqtt.hrl").

-record(mqtt_presence, {user,clients,last_status}).

-create_mnesia_tables({mnesia,[create_prescence]}).
-clear_mnesia_table({mnesia,[clear_table]}).

%% @doc Mnesia table manipulations.
mnesia(create_prescence) ->
    mnesia:create_table(mqtt_presence, [{ram_copies, [node()]}, {attributes, record_info(fields,mqtt_presence)}, {type,set}]),
    ok = mnesia:wait_for_tables([mqtt_presence], 6000);
mnesia(clear_table) ->
    Transaction = fun() ->
        mnesia:clear_table(mqtt_presence)
    end,
    mnesia:transaction(Transaction);
mnesia(Action) ->
    mqtt_server:mnesia(Action).

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

-define(USER_ID_REGEX, "([a-zA-Z0-9]+)").
-define(THREAD_ID_REGEX, "([a-zA-Z_0-9]+)").
-define(THREAD_REGEX, snd(re:compile("threads/" ++ ?THREAD_ID_REGEX ++ "/messages"))).
-define(ONLINE_REGEX, snd(re:compile("online/" ++ ?USER_ID_REGEX))).

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
    mqtt_server:init(Params ++ [{data, [{objectid, objectid:objectid()}]}]).

persist_message(Message,Context,Session,ThreadId,Body,Data) ->
    {_, ObjectId} = proplists:lookup(objectid, Data),
    {Id, NextId} = ObjectId,
    {ok,State} = gen_server:call(Session,state),
    Username = mqtt_session:username(State),
    '3rd-base_db_utils':store_message(Id, Body, ThreadId, Username),
    NewData = [{objectid, NextId()}|proplists:delete(objectid,Data)],
    mqtt_server:handle_message(Message,Context#?CONTEXT{data=NewData}).

online_status(Status,UserId,ClientId) ->
    case Status of
        <<"offline">> ->
            case logoff(UserId,ClientId) of
                {atomic, NumClients} when NumClients =:= 0 ->
                    % No more clients, let subscribers know this user is
                    % offline now
                   reply;
                _Otherwise ->
                    noreply
            end;
        _ ->
            case update_status(Status,UserId,ClientId) of
                {atomic, {PrevStatus,_Clients}} when PrevStatus =/= Status ->
                    reply;
                _ ->
                   noreply
            end
    end.

update_status(Status,UserId,ClientId) ->
    Transaction = fun() ->
            case mnesia:read(mqtt_presence,UserId,write) of
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
                    sets:size(NewClients)
                    ;
                _DoesNotExist ->
                    mnesia:abort("No such presence object")
            end
    end,
    mnesia:transaction(Transaction).

runall(Subject, [{Name, RE} | REs], Options) ->
    case re:run(Subject, RE, Options) of
        {match, Captured} -> {Name, Captured};
        nomatch -> runall(Subject, REs, Options)
    end;
runall(_Subject, [], _Options) ->
    nomatch.

% Incoming messages
-spec handle_message(mqtt_message(), context()) ->
    {reply, mqtt_message(), context(), timeout()} |
    {noreply, context(), timeout()} |
    {stop, Reason :: term()}.
handle_message(
            Message=#mqtt_publish{topic = Topic, payload=Payload},
            Context=#?CONTEXT{session=Session, data=Data, client_id=ClientId}
            ) when is_pid(Session) ->
    RegOpts = [{capture,all_but_first,binary}],
    case runall(Topic, [{thread, ?THREAD_REGEX},
                        {online, ?ONLINE_REGEX}], RegOpts) of
        {online, [UserId]} ->
            {JSON} = jiffy:decode(Payload),
            Status = proplists:get_value(<<"status">>, JSON),
            case online_status(Status,UserId,ClientId) of
                reply -> mqtt_server:handle_message(Message,Context);
                noreply -> {noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout}
            end;
        {thread, [ThreadId]} ->
            {JSON} = jiffy:decode(Payload),
            Body = proplists:get_value(<<"body">>, JSON),
            persist_message(Message,Context,Session,ThreadId,Body,Data);
        nomatch ->
            mqtt_server:handle_message(Message,Context)
    end;

handle_message(Message,Context) ->
    mqtt_server:handle_message(Message,Context).

handle_will_message(ClientId,Message=#mqtt_publish{topic=Topic, payload=Payload}) ->
    RegOpts = [{capture,all_but_first,binary}],
    case runall(Topic, [{online, ?ONLINE_REGEX}], RegOpts) of
        {online, [UserId]} ->
            {JSON} = jiffy:decode(Payload),
            Status = proplists:get_value(<<"status">>, JSON),
            case online_status(Status,UserId,ClientId) of
                reply -> {reply, Message};
                noreply -> noreply
            end
    end;
handle_will_message(ClientId, Message) ->
    mqtt_server:handle_will_message(ClientId, Message).

% Outgoing messages
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
