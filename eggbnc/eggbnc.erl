-module(eggbnc).

-author('A.V. Lukyanov <lomka@gero.in>').

-export([start/0,dump/0,cleanup/1,kill/1,get_all/1]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-record(sessions,{jid,pid,seen,session}).
-record(messages,{id,stamp,msg}).

%% user config
-define(RECV_TIMEOUT,20000).
-define(RECONNECT_TIME,30000).
-define(LISTEN_ADDR,{0,0,0,0}).
-define(LISTEN_PORT,5222).
-define(LIMIT_DOMAIN,<<"example.com">>).
server(J,P) ->
						%io:format("server con~n"),
    S = exmpp_session:start({1,0}),
    exmpp_session:auth_info(S,J,P),
    [{Host,Port}|_] = exmpp_dns:get_c2s("gmail.com"),
    {ok,_,_} = exmpp_session:connect_TCP(S,Host,Port,[{starttls,enabled}]),
    {ok,_ServerJID} = exmpp_session:login(S,"PLAIN"),
    S.
%%

getandparse(S,D) ->
    {ok,Data} = gen_tcp:recv(S,0,?RECV_TIMEOUT),
						%CleanData = re:split(Data,"\\n",[{return,binary},trim]),
						%io:format("recv from client ~p~n", [Data]),
    case exmpp_xml:parse_document_fragment(Data,[{root_depth,D}]) of
	continue ->
	    getandparse(S,D);
	E ->
	    [Elements] = exmpp_xml:remove_whitespaces_from_list(E),
	    Elements
    end.
client(S) ->
    Opening = getandparse(S,1),
    Domain = exmpp_stream:get_receiving_entity(Opening),

    %% COMMENT to allow all domains server handles
    ?LIMIT_DOMAIN = Domain,

    ok = send(S,exmpp_stream:opening_reply(Opening,random)),
    ok = send(S,exmpp_stream:features(exmpp_server_sasl:feature(["PLAIN"]))),
    {auth,_,Challenge} = exmpp_server_sasl:next_step(getandparse(S,0)),
    [_Domain,Login,Password] = binary:split(list_to_binary(Challenge),[<<0>>],[global]),
    ok = send(S,exmpp_server_sasl:success()),
    ok = send(S,exmpp_stream:opening_reply(getandparse(S,1),random)),
    ok = send(S,exmpp_stream:features([exmpp_server_binding:feature(),exmpp_server_session:feature()])),
    Bind = getandparse(S,0),
    %% for clients like os x messages
    BindWorkaround = Bind#xmlel{ns=?NS_JABBER_CLIENT},

    JID = exmpp_jid:make(binary_to_list(Login),binary_to_list(Domain),make_resource(BindWorkaround)),
    {RemoteHandler,RemoteSession,TableName} = connect(JID,binary_to_list(Password)),

    ok = send(S,exmpp_server_binding:bind(BindWorkaround,JID)),
    Session = getandparse(S,0),
    SessionWorkaround = Session#xmlel{ns=?NS_JABBER_CLIENT},
    %% psi wants to set session even if we don't advert this feature
    ok = send(S,exmpp_server_session:establish(SessionWorkaround)),

    %% todo put in more appropriate place
    send_messages(TableName,S),

    P = exmpp_xml:start_parser([{root_depth,1}]),
    %% since root elm is lost
    exmpp_xml:parse(P,"<stream:stream xmlns:stream='fake'>"),

    RemoteHandler ! self(),

    %% quickly set off to be notified of roster people presences
    send_packet(RemoteSession,exmpp_presence:set_status(exmpp_presence:unavailable(),"")),

    client_handler(S,RemoteSession,P).

send(S,D) ->
    Binary = exmpp_stream:to_binary(D),
						%io:format("to client ~p~n", [Binary]),
    gen_tcp:send(S,Binary).
send_packet(S,P) ->
						%io:format("to server ~p~n",[exmpp_stream:to_binary(P)]),
    exmpp_session:send_packet(S,P).
make_resource(B) ->
    case exmpp_server_binding:wished_resource(B) of
	undefined ->
	    "eggbouncer";
	R ->
	    R
    end.
send_messages(T,S) ->
    case dirty_get_all(T) of
	[] -> true;
	M ->
						%io:format("off msgs ~p~n", M),
	    lists:foreach(fun(X) -> send(S,add_delayed(X#messages.stamp,X#messages.msg)) end,lists:sort(M)),
	    mnesia:clear_table(T),
	    mnesia:dump_tables([T])
    end.
add_delayed(TS,#xmlel{ns = NS} = Message) ->
    case exmpp_xml:get_element(Message,NS,'delay') of
	undefined ->
	    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
	    Stamp = io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",[Year,Month,Day,Hour,Minute,Second]),
	    Delayed = #xmlel{ns=?NS_DELAY,name='delay',attrs=[exmpp_xml:attribute(<<"stamp">>,Stamp)]},
	    exmpp_xml:append_child(Message,Delayed);
	_ ->
	    Message
    end.
get_time() ->
    {Mega,Secs,_} = os:timestamp(),
    Mega*1000000 + Secs.
bnc_status(S) ->
    send_packet(S,exmpp_presence:set_status(exmpp_presence:set_show(exmpp_presence:available(),'xa'),"eggbouncer")).
reconnect(SL,T) ->
    S = SL(),
    {_,ok} = update_session(T,S),
    monitor(process,S),
    S.

start() ->
    exmpp:start(),
    init_db(),
    listen().

listen() ->
    {ok,Socket} = gen_tcp:listen(?LISTEN_PORT,[binary,{packet,0},{active,false},{reuseaddr,true},{ip,?LISTEN_ADDR}]),
    accept(Socket).
accept(LS) ->
    case gen_tcp:accept(LS) of
	{ok,Socket} ->
	    Pid = spawn(fun() -> client(Socket) end),
	    gen_tcp:controlling_process(Socket,Pid),
	    accept(LS);
	%% probably too much
	_Catchall ->
	    timer:sleep(60000),
	    accept(LS)
    end.

connect(J,P) ->
    TableName = binary_to_atom(binary:replace(base64:encode(exmpp_jid:to_binary(J)),<<"/">>,<<"_">>,[global]),latin1),
    case maybe_kill_session(find_session(TableName)) of
	[Record] ->
	    exmpp_session:stop(server(exmpp_jid:bare(J),P)),
	    {_,ok} = update_seen(TableName,get_time()),
	    {Record#sessions.pid,Record#sessions.session,TableName};
	[] ->
	    ServerLambda = fun() -> server(J,P) end,
	    Session = ServerLambda(),
	    Pid = spawn(fun() -> monitor(process,Session), server_handler([],0,TableName,ServerLambda,?RECONNECT_TIME) end),
	    %% todo kill proc on error
	    ok = exmpp_session:set_controlling_process(Session,Pid),
	    {_,ok} = insert_session(TableName,Pid,Session,get_time()),
	    %% eating atoms space - table per jid for safe dirty ops
	    mnesia:create_table(TableName,[{record_name,messages},{attributes,record_info(fields,messages)}]),
	    {Pid,Session,TableName}
    end.

handle_packet(Packet,S) ->
    exmpp_session:send_packet(S,Packet).
%% todo catch pings, stream ends, spoof from/to for server changed resources
client_handler(So,Se,P) ->
    inet:setopts(So,[{active,once}]),
    receive
	{tcp,So,Data} ->
	    case exmpp_xml:parse(P,Data) of
		continue ->
		    true;
		E ->
		    Elements = exmpp_xml:remove_whitespaces_from_list(E),
						%io:format("from client ~p~n", [Data]),
		    lists:foreach(fun(X) -> handle_packet(X,Se) end,Elements)
	    end,
	    client_handler(So,Se,P);
	{tcp_closed,So} ->
						%io:format("client ~p discon~n", [So]),
	    exmpp_xml:stop_parser(P),
	    bnc_status(Se);
	{tcp_error,So,_Reason} ->
						%io:format("error client ~p sock ~p~n", [So,Reason]),
	    exmpp_xml:stop_parser(P),
	    bnc_status(Se);
	Packet = #xmlel{} ->
						%send(So,Packet),
	    gen_tcp:send(So,exmpp_stream:to_binary(Packet)),
	    client_handler(So,Se,P);
	stop ->
	    exmpp_xml:stop_parser(P);
	_Catchall ->
						%io:format("client catchall ~p~n",[_Catchall]),
	    client_handler(So,Se,P)
    end.

server_handler(P,Id,T,SL,R) ->
    receive
	Record = #received_packet{raw_packet=Packet} ->
	    case is_process_alive(P) of
		true ->
						%P ! exmpp_xml:set_attribute(Packet,<<"to">>,FullJID),
		    P ! Packet,
		    server_handler(P,Id,T,SL,R);
		%% COMMENT when guard for off storing of other types of stanza
		false when Record#received_packet.packet_type == message andalso
			   Record#received_packet.type_attr =/= "error" ->
		    case exmpp_message:get_body(Packet) of
			undefined ->
			    server_handler(P,Id,T,SL,R);
			_ when Record#received_packet.type_attr == "groupchat" ->
			    server_handler(P,Id,T,SL,R);
			_ ->
			    insert_message(T,Id,Packet),
			    server_handler(P,Id+1,T,SL,R)
		    end;
		false when	Record#received_packet.packet_type == 'presence' andalso
				(Record#received_packet.type_attr == "subscribe" orelse
				 Record#received_packet.type_attr == "subscribed") ->
		    insert_message(T,Id,Packet),
		    server_handler(P,Id+1,T,SL,R);
		_Catchall ->
						%io:format("server packet catch all ~p~n",[Record]),
		    server_handler(P,Id,T,SL,R)
	    end;
	P1 when is_pid(P1) ->
	    server_handler(P1,0,T,SL,R);
	{'DOWN',_,_,_,_} ->
	    P ! stop,
	    update_session(T,'session-in-restart'),
	    self() ! restart,
	    server_handler(P,Id,T,SL,R);
	restart ->
	    io:format("~p server recon~n", [T]),
	    %% UNCOMMENT for delay
	    %%timer:sleep(R),
	    try reconnect(SL,T) of
		NewS ->
		    io:format("~p session resurrected~n", [T]),
		    bnc_status(NewS),
		    server_handler(P,Id,T,SL,?RECONNECT_TIME)
		    %% probably too much
	    catch
		_Catchall ->
		    self() ! restart,
		    server_handler(P,Id,T,SL,R*2)
	    end;
	stop ->
	    P ! stop;
	_Catchall ->
						%io:format("server catchall ~p~n",[_Catchall]),
	    server_handler(P,Id,T,SL,R)
    end.

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(sessions,[{attributes,record_info(fields,sessions)}]).
get_all(T) ->
    F = fun() -> mnesia:select(T,[{'_',[],['$_']}]) end,
    {atomic,MR} = mnesia:transaction(F),
    MR.
dirty_get_all(T) ->
    try mnesia:dirty_select(T,[{'_',[],['$_']}]) of
	All ->
	    All
    catch
	_ ->
	    []
    end.
find_session(J) ->
    F = fun() -> mnesia:match_object(#sessions{jid=J,_='_'}) end,
    {atomic,MR} = mnesia:transaction(F),
    MR.
insert_session(J,P,S,T) ->
    F = fun() -> mnesia:write(#sessions{jid=J,pid=P,session=S,seen=T}) end,
    mnesia:transaction(F).
update_session(J,S) ->
    [MR] = find_session(J),
    F = fun() -> mnesia:write(MR#sessions{session=S}) end,
    mnesia:transaction(F).
kill_session(MR) ->
    MR#sessions.pid ! stop,
    exmpp_session:stop(MR#sessions.session),
    F = fun() -> mnesia:delete_object(MR) end,
    mnesia:transaction(F),
    mnesia:clear_table(MR#sessions.jid).
%% todo supervise and relaunch server proc instead
maybe_kill_session([MR]) ->
    case is_process_alive(MR#sessions.pid) of
	true ->
	    [MR];
	false ->
	    exmpp_session:stop(MR#sessions.session),
	    F = fun() -> mnesia:delete_object(MR) end,
	    mnesia:transaction(F),
	    []
    end;
maybe_kill_session([]) ->
    [].
find_seen(T) ->
    F = fun() -> mnesia:select(sessions,[{#sessions{seen='$1',_='_'},[{'<','$1',T}],['$_']}]) end,
    {atomic,MR} = mnesia:transaction(F),
    MR.
update_seen(J,T) ->
    [MR] = find_session(J),
    F = fun() -> mnesia:write(MR#sessions{seen=T}) end,
    mnesia:transaction(F).
insert_message(T,I,P) ->
    catch mnesia:dirty_write(T,#messages{id=I,stamp=os:timestamp(),msg=P}).

dump() ->
    Tables = lists:filter(fun(X) -> mnesia:table_info(X,record_name) == messages end,mnesia:system_info(tables)),
    io:format("dumping ~p~n",[Tables]),
    mnesia:dump_tables(Tables).
cleanup(T) ->
    Tables = find_seen(get_time() - T),
    io:format("clearing ~p~n",[Tables]),
    lists:foreach(fun(X) -> kill_session(X) end,Tables).
kill(J) ->
    [MR] = find_session(J),
    kill_session(MR).
