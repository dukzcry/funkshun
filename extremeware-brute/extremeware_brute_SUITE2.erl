-module(extremeware_brute_SUITE2).
-export([all/0,suite/0,main/1]).

-record(settings,{threads,limit,command,error,error_mp=undefined,positions=undefined,ssh_conn=undefined}).

suite() ->
	[{timetrap,infinity}].%,{silent_connections,[ssh]}].
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
	Settings = PreSettings#settings{error_mp=Mp,positions=length(integer_to_list(PreSettings#settings.limit))},
	Threads = Settings#settings.threads, Limit = Settings#settings.limit,
	NZeroIncl = Threads - 1, NWOLast = NZeroIncl - 1,
	Size = Limit div Threads, Delta = Limit - (Size * Threads),
	%[I] = [ Clp ||
	%			Clp <- lists:seq(0,NWOLast),
	%			part(Clp,Size,0,Settings) > NWOLast ],
	%ct:pal("Generated fragments"),
	{ok,Handle} = ct_ssh:connect(unix),
	Pids = lists:map(fun(X) -> part(X,Size,0,Settings#settings{ssh_conn=Handle}) end,lists:seq(0,NWOLast)),
	Last = part(NZeroIncl,Size,Delta,Settings),
	loop([Last|Pids],Settings).

part(N,S,D,Settings) ->
	M = N * S,
	Fragment = lists:seq(M + 1,M + S + D),
	FragmentOptimized = lists:map(fun(X) ->
		lists:reverse(string:right(integer_to_list(X),Settings#settings.positions,$0)) end,Fragment),
	%timer:sleep(1000),
	Pid = spawn_link(fun() -> worker({FragmentOptimized},Settings) end),
	[F|_] = FragmentOptimized,
	ct:pal("Thread ~w starts at ~s (~w)",[Pid,F,M + 1]),
	Pid.
worker({L},Settings) ->
	receive
		stop ->
			ct:pal("Halting"),
			true
		after 0 ->
			{ok,Handler} = ct_ssh:session_open(Settings#settings.ssh_conn),
			worker({Handler,L},Settings)
	end;
worker({Handler,[X|Xs]},Settings) ->
	receive
		stop ->
			ct:pal("Halting on ~s",[X]),
			worker({Handler,[]},Settings)
		after 0 ->
			%timer:sleep(500),
			{ok,Data} = ct_ssh:exec(Settings#settings.ssh_conn,Handler,Settings#settings.command ++ " " ++ X,10000),
			case re:run(Data,Settings#settings.error_mp,[{capture,none}]) of
				match ->
					worker({Handler,Xs},Settings);
				_ ->
					ct:pal("Done! ~s",[Data]), ct_ssh:session_close(Handler), Data = fail
				end
	end;
worker({Handler,[]},_) ->
	ct_ssh:session_close(Handler), true.
loop([X|Xs],Settings) ->
	receive
		{_,Pid,normal} ->
			ct:pal("~w finished normally",[Pid]),
			loop(check([X|Xs]),Settings);
		{_,Pid,_} ->
			ct:pal("~w finished early",[Pid]),
			%[ LP ! stop || LP <- begin {links, P} = process_info(self(), links), P end ],
			lists:foreach(fun(Y) -> Y ! stop end,[X|Xs]),
			timer:sleep(500 * Settings#settings.threads),
			loop([],Settings)
		after 0 ->
			loop(check([X|Xs]),Settings)
	end;
loop([],Settings) ->
	ct_ssh:disconnect(Settings#settings.ssh_conn), true.
check([X|Xs]) ->
	case is_process_alive(X) of
		false -> Xs;
		true -> Xs ++ [X]
	end.
