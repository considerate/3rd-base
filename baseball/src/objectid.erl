-module(objectid).
-export([objectid/2, objectid/1, objectid/0, objectid_time/1]).

-define(MAX_INC, round(math:pow(2,24)-1)).

% objectid is 96 bits = 12 Bytes containing: <<time:32, pid/machine:40, inc:24>>
% Pid in this case is self(). 
- type objectid() :: <<_:96>>.
- spec objectid(pid(), integer()) -> objectid().
objectid(Pid, Inc) ->
	Bin_Pid = term_to_binary(Pid),
	Seconds = time_to_sec(),
	<<Seconds:32, Bin_Pid:5/binary, Inc:24>>.

- spec objectid() -> {objectid(), fun(() -> objectid())}.
objectid() ->
	objectid(self()).

- spec objectid(pid()) -> {objectid(), fun(() -> objectid())}.
objectid(Pid) -> 
	Start = random:uniform(?MAX_INC),
	{objectid(Pid,Start), nextobjectid(Pid,Start+1)}.

- spec nextobjectid(pid(), integer()) -> {objectid(), fun(()-> objectid())}.
nextobjectid(Pid,Inc) -> 
	fun() ->
		{objectid(Pid,Inc rem ?MAX_INC), nextobjectid(Pid,Inc+1)}
	end.	

% Tells the time the object was created, {{year1970(), month(), day()}, time()}
- spec objectid_time(objectid()) -> calendar:datetime1970().
objectid_time(<<Time:32, _:64>>) -> 
	sec_to_real_time(Time).

% Time manipulative functions for objectid.

% Gets the time in seconds to use in objectid.
- type time_to_sec() :: integer().
time_to_sec() -> 
	{MegaSecs, Secs, _} = now(),
	MegaSecs * 1000000 + Secs.

% Converts time in objectid to real time.
- spec sec_to_real_time(integer()) -> calendar:datetime1970().
sec_to_real_time(Time) -> 
	Time_Now = {Time div 1000000, Time rem 1000000, 0},
	calendar:now_to_local_time(Time_Now).	

	