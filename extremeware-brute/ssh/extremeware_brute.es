#!/usr/bin/env escript
%%! -smp enable
-mode(native).

-record(settings,{limit=9999999,command="enable license fullL3 ",error="ERROR",
	recv_timeout=1000,ssh=[{silently_accept_hosts,true},{connect_timeout,10000},{compression,none}],

    error_mp=undefined,ssh_conn=undefined}).

main([ConToAddr,Port,User,Pass,Threads,NewConnAt]) ->
    process_flag(trap_exit, true), crypto:start(), ssh:start(),
    N = list_to_integer(Port), T = list_to_integer(Threads), A = list_to_integer(NewConnAt),
    PreSettings = #settings{},
    {ok,Mp} = re:compile(PreSettings#settings.error),
    Limit = PreSettings#settings.limit,
    NZeroIncl = T - 1, NWOLast = NZeroIncl - 1,
    Size = Limit div T, Delta = Limit - (Size * T),
    %[I] = [ Clp ||
    %			Clp <- lists:seq(0,NWOLast),
    %			part(Clp,Size,0,Settings) > NWOLast ],
    %io:format("Generated fragments~n"),
    Ssh = [{user,User}
        % Interactive asking fails
        ,{password,Pass}
        |PreSettings#settings.ssh],
    {ok,Handle} = ssh:connect(ConToAddr,N,Ssh),
    Settings = PreSettings#settings{error_mp=Mp,ssh_conn=Handle},
    io:format("~w~w~n", [date(),time()]),
    Partionize = fun(C,L) -> Fun = fun
                (_,_,[]) ->
                    [];
                (F,Conn,[X|Xs]) ->
                    Expr = (A + (X + 1)) rem A =:= 0,
                    if Expr ->
                        %io:format("New connection~n"),
                        {ok,NewHandle} = ssh:connect(ConToAddr,N,Ssh),
                        NewConn = NewHandle;
                    true ->
                        NewConn = Conn
                    end,
                    Pid = part(X,Size,0,Settings#settings{ssh_conn=NewConn}),
                    [Pid | F(F,NewConn,Xs)]
        end,
        Fun(Fun,C,L)
    end,
    Pids = Partionize(Handle,lists:seq(0,NWOLast)),
    Last = part(NZeroIncl,Size,Delta,Settings),
    loop([Last|Pids],Settings),
    io:format("~w~w~n", [date(),time()]);
main(_) ->
    io:format("~s Host Port Login Pass 20 10\n", [escript:script_name()]),
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
loop([],_) ->
	true.

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
            {ok,_Prompt} = ssh_loop(Settings#settings.ssh_conn,Handler,[],Settings),
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
				Settings#settings.command++integer_to_list(X)++"\n",10000),
            {Status,Data} = ssh_loop(Settings#settings.ssh_conn,Handler,[],Settings),
            case re:run(Data,Settings#settings.error_mp,[{capture,none}]) of
                match ->
                    worker({Handler,Xs},Settings);
                _ ->
                    if Data /= [] -> io:format("Done! ~s~n",[Data]); true -> true end,
                    ssh_connection:close(Settings#settings.ssh_conn,Handler), Data = Status
            end
    end;
worker({_,[]},Settings) ->
    %ssh_connection:close(Settings#settings.ssh_conn,Handler)
    ssh:close(Settings#settings.ssh_conn), true.
check([X|Xs]) ->
    case is_process_alive(X) of
        false -> Xs;
        true -> Xs ++ [X]
    end.
ssh_loop(SSH,Chn,Data,Settings) ->
    receive
        {ssh_cm,SSH,{data,Chn,_,NewData}} ->
            ssh_connection:adjust_window(SSH,Chn,size(NewData)),
            DataAcc = Data ++ binary_to_list(NewData),
            ssh_loop(SSH,Chn,DataAcc,Settings);
        %{ssh_cm,SSH,{eof,Chn}} ->
            %{ok,Data};
        stop ->
            {ok,Data};
        {ssh_cm,SSH,State} ->
            %io:format("State: ~w~n", [State]),
            {fail,[]}
        % Sensitive
        after Settings#settings.recv_timeout ->
            {ok,Data}
    end.
