#!/usr/bin/env escript
%%! -smp enable
-mode(native).

-define(CMD_TIMEOUT,1000).
-record(settings,{limit=9999999,command="enable license fullL3 ",error="ERROR",
	  ssh=[{silently_accept_hosts,true},{connect_timeout,10000},{compression,none}],

        error_mp=undefined,ssh_conn=undefined}).

main([ConToAddr,Port,User,Pass,Threads]) ->
    process_flag(trap_exit, true), crypto:start(), ssh:start(),
    N = list_to_integer(Port), T = list_to_integer(Threads),
    PreSettings = #settings{},
    {ok,Mp} = re:compile(PreSettings#settings.error),
    Limit = PreSettings#settings.limit,
    NZeroIncl = T - 1, NWOLast = NZeroIncl - 1,
    Size = Limit div T, Delta = Limit - (Size * T),
    %[I] = [ Clp ||
    %			Clp <- lists:seq(0,NWOLast),
    %			part(Clp,Size,0,Settings) > NWOLast ],
    %io:format("Generated fragments~n"),
    {ok,Handle} = ssh:connect(ConToAddr,N,[{user,User}
        % Interactive asking fails
        ,{password,Pass}
        |PreSettings#settings.ssh]),
    Settings = PreSettings#settings{error_mp=Mp,ssh_conn=Handle},
    Pids = lists:map(fun(X) -> part(X,Size,0,Settings) end,lists:seq(0,NWOLast)),
    Last = part(NZeroIncl,Size,Delta,Settings),
    loop([Last|Pids],Settings);
main(_) ->
    io:format("~s Host Port Login Pass 10\n", [escript:script_name()]),
    halt(1).

loop([X|Xs],Settings) ->
    receive
        {_,Pid,normal} ->
            %io:format("~w finished normally~n",[Pid]),
            loop(check([X|Xs]),Settings);
        {_,Pid,_} ->
            %io:format("~w finished early~n",[Pid]),
            %[ LP ! stop || LP <- begin {links, P} = process_info(self(), links), P end ],
            lists:foreach(fun(Y) -> Y ! stop end,[X|Xs]),
            timer:sleep(3000),
            loop([],Settings)
        after 0 ->
            loop(check([X|Xs]),Settings)
    end;
loop([],Settings) ->
	ssh:close(Settings#settings.ssh_conn), true.

part(N,S,D,Settings) ->
    M = N * S,
    Fragment = lists:seq(M + 1,M + S + D),
    Pid = spawn_link(fun() -> worker({Fragment},Settings) end),
    io:format("Thread ~w starts at ~w~n",[Pid,M + 1]),
    Pid.
worker({L},Settings) ->
    receive
        stop ->
            %io:format("Halting~n"),
            true
        after 0 ->
            {ok,Handler} = ssh_connection:session_channel(Settings#settings.ssh_conn,infinity),
            success = ssh_connection:open_pty(Settings#settings.ssh_conn,Handler,"dumb",1,1,[],infinity),
            ok = ssh_connection:shell(Settings#settings.ssh_conn,Handler),
            {ok,_Prompt} = ssh_loop(Settings#settings.ssh_conn,Handler,[]),
            worker({Handler,L},Settings)
    end;
worker({Handler,[X|Xs]},Settings) ->
    receive
        stop ->
            %io:format("Halting on ~w~n",[X]),
            worker({Handler,[]},Settings)
        after 0 ->
            %timer:sleep(500),
            ok = ssh_connection:send(Settings#settings.ssh_conn,Handler,
				Settings#settings.command++integer_to_list(X)++"\n",?CMD_TIMEOUT),
            {Status,Data} = ssh_loop(Settings#settings.ssh_conn,Handler,[]),
            case re:run(Data,Settings#settings.error_mp,[{capture,none}]) of
                match ->
                    worker({Handler,Xs},Settings);
                _ ->
                    if Data /= [] -> io:format("Done! ~s~n",[Data]) end, 
                    ssh_connection:close(Settings#settings.ssh_conn,Handler), Data = Status
            end
    end;
worker({Handler,[]},Settings) ->
    ssh_connection:close(Settings#settings.ssh_conn,Handler), true.
check([X|Xs]) ->
    case is_process_alive(X) of
        false -> Xs;
        true -> Xs ++ [X]
    end.
ssh_loop(SSH,Chn,Data) ->
    receive
        {ssh_cm,SSH,{data,Chn,_,NewData}} ->
            ssh_connection:adjust_window(SSH,Chn,size(NewData)),
            DataAcc = Data ++ binary_to_list(NewData),
            ssh_loop(SSH,Chn,DataAcc);
        %{ssh_cm,SSH,{eof,Chn}} ->
            %{ok,Data};
        stop ->
            {ok,Data};
        State ->
            %io:format("State: ~w~n", [State]),
            {fail,[]}
        after ?CMD_TIMEOUT ->
            {ok,Data}
    end.
