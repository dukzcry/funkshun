%%! -enable smp
% ct_run -suite

-module(extremeware_brute_SUITE).
-export([all/0,suite/0,main/1]).

-record(settings,{contoaddr="localhost",login="login",passwd="pass",threads=5}).

-record(parms,{timeout=10,limit=500}).

% Global settings
suite() ->
	Settings = #settings{},
 	[{require, unix_telnet, unix}, 
	 {default_config, unix, [{telnet,Settings#settings.contoaddr}, 
				 {username,Settings#settings.login}, 
				 {password,Settings#settings.passwd}]}].

all() -> [main].
main(_) ->
	Settings = #settings{},
	Parms = #parms{},
	Threads = Settings#settings.threads,
	NZeroIncl = Threads - 1, NWOLast = NZeroIncl - 1,
	Size = Parms#parms.limit div Threads, Delta = Parms#parms.limit - (Size * Threads),
	Range = lists:seq(0,NWOLast),
	[I] = [ Clp ||
				Clp <- Range,
				part(Clp,Size,0) > NWOLast ],
	part(I + 1,Size,Delta),
	loop(Range),
	ok.

part(N,S,D) ->
	M = N * S,
	Fragment = lists:seq(M + 1,M + S + D),
    Pid = spawn(fun() -> worker({Fragment}) end),
	register(list_to_atom(integer_to_list(N)),Pid),
	N + 1.
worker({L}) ->
	{ok,Handler} = ct_telnet:open(unix_telnet),
	worker({Handler,L});
worker({Handler,[X|Xs]}) ->
	{ok,_Result} = ct_telnet:cmd(Handler,"ls"),
	worker({Handler,Xs});
worker({Handler,[]}) ->
	ok = ct_telnet:close(Handler).
loop([N|Ns]) ->
	Atom = list_to_atom(integer_to_list(N)),
	case whereis(Atom) of
		undefined -> L = [];
		_ -> L = Ns ++ [N]
	end,
	loop(L);
loop([]) ->
	true.
