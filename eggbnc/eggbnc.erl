-module(eggbnc).

-author('A.V. Lukyanov <lomka@gero.in>').

-export([start/0,dump/0,cleanup/1]).

% user config
-define(RECV_TIMEOUT,20000).
-define(RECONNECT_TIME,30000).
-define(LISTEN_ADDR,{127,0,0,1}).
-define(LISTEN_PORT,5222).
-define(LIMIT_DOMAIN,<<"gero.in">>).
server(J,P) ->
	 %io:format("server con~n"),
	 % snapshot ver of exmpp is required for gtalk conn
	 S = exmpp_session:start({1,0}),
	 exmpp_session:auth_info(S,J,P),
	 [{Host,Port}|_] = exmpp_dns:get_c2s("gmail.com"),
	 {ok,_,_} = exmpp_session:connect_TCP(S,Host,Port,[{starttls,enabled}]),
	 {ok,_ServerJID} = exmpp_session:login(S,"PLAIN"),
	 S.
%

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
	       _Catchall ->
	       timer:sleep(60000),
	       accept(LS)
	  end.

-record(sessions,{jid,pid,session,seen}).
-record(messages,{id,msg}).

get_time() ->
	   {_,Secs,_} = os:timestamp(),
	   Secs.
table_name(J) ->
	Encoded = binary:replace(base64:encode(exmpp_jid:to_binary(J)),<<"/">>,<<"_">>,[global]),
	binary_to_atom(Encoded,latin1).

connect(J,P) ->
	TableName = table_name(J),
	ServerLambda = fun(J_) -> server(J_,P) end,
	Session = ServerLambda(J),
	case find_session(J) of
	     [Record] ->
	     	exmpp_session:stop(Session),
		{_,ok} = update_seen(J,get_time()),
		{Record#sessions.pid,Record#sessions.session,TableName};
	     [] ->
	     	Pid = spawn(fun() -> monitor(process,Session), server_handler([],0,J,TableName,ServerLambda) end),
		% todo kill proc on error
		ok = exmpp_session:set_controlling_process(Session,Pid),
	     	{_,ok} = insert_session(J,Pid,Session,get_time()),
		% eating atoms space - table per jid for safe dirty ops
		mnesia:create_table(TableName,[{record_name,messages},{attributes,record_info(fields,messages)}]),
		{Pid,Session,TableName}
	end.

getandparse(S,D) ->
       {ok,Data} = gen_tcp:recv(S,0,?RECV_TIMEOUT),
       %CleanData = re:split(Data,"\\n",[{return,binary},trim]),
       %io:format("got recv ~p~n", [CleanData]),
       case exmpp_xml:parse_document_fragment(Data,[{root_depth,D}]) of
       	    continue ->
	    	     getandparse(S,D);
	    E ->
		     [Elements] = exmpp_xml:remove_whitespaces_from_list(E),
		     Elements
       end.
send(S,D) ->
	  gen_tcp:send(S,exmpp_stream:to_binary(D)).
-include_lib("exmpp/include/exmpp.hrl").
client(S) ->
	 Opening = getandparse(S,1),
	 Domain = exmpp_stream:get_receiving_entity(Opening),
	 % comment to allow all domains server handles
	 ?LIMIT_DOMAIN = Domain,
	 ok = send(S,exmpp_stream:opening_reply(Opening,random)),
	 ok = send(S,exmpp_stream:features(exmpp_server_sasl:feature(["PLAIN"]))),
	 {auth,_,Challenge} = exmpp_server_sasl:next_step(getandparse(S,0)),
	 [_Domain,Login,Password] = binary:split(list_to_binary(Challenge),[<<0>>],[global]),
	 ok = send(S,exmpp_server_sasl:success()),

	 ok = send(S,exmpp_stream:opening_reply(getandparse(S,1),random)),
	 ok = send(S,exmpp_stream:features([exmpp_server_binding:feature(),exmpp_server_session:feature()])),
	 Bind = getandparse(S,0),
	 BindWorkaround = Bind#xmlel{ns = ?NS_JABBER_CLIENT},
	 Resource = case exmpp_server_binding:wished_resource(BindWorkaround) of
	 	    	 undefined ->
	 		 	   "eggbouncer";
	 		 R ->
	 		 	  R
	 	    end,
	 JID = exmpp_jid:make(binary_to_list(Login),binary_to_list(Domain),Resource),
	 {RemoteHandler,RemoteSession,TableName} = connect(JID,binary_to_list(Password)),
	 ok = send(S,exmpp_server_binding:bind(BindWorkaround,JID)),
	 Session = getandparse(S,0),
	 SessionWorkaround = Session#xmlel{ns = ?NS_JABBER_CLIENT},
	 % psi wants to set session even if we don't advert this feature
	 ok = send(S,exmpp_server_session:establish(SessionWorkaround)),

	 % todo put in better place
	 case get_messages(TableName) of
	      [] -> true;
	      M ->
	      	%io:format("off msgs ~p~n", M),
	      	lists:foreach(fun({_,_,Message}) -> send(S,Message) end,lists:sort(M)),
		mnesia:clear_table(TableName), mnesia:dump_tables([TableName])
	 end,

	 P = exmpp_xml:start_parser([{root_depth,1}]),
	 % since root elm is lost
	 exmpp_xml:parse(P,"<stream:stream xmlns:stream='fake'>"),
	 RemoteHandler ! self(),
	 % quickly set off to be notified of roster people presences
	 exmpp_session:send_packet(RemoteSession,exmpp_presence:set_status(exmpp_presence:unavailable(),"")),
	 client_handler(S,RemoteSession,P).

bnc_status(S) ->
	   exmpp_session:send_packet(S,exmpp_presence:set_status(exmpp_presence:set_show(exmpp_presence:available(),'xa'),"eggbouncer")).

% todo catch pings, stream ends, spoof from/to for server changed resources
client_handler(So,Se,P) ->
	 inet:setopts(So,[{active,once}]),
	 receive
	 {tcp,So,Data} ->
	 	      %io:format("client ~p~n", [Data]),
		      % todo send it raw instead
		      case exmpp_xml:parse(P,Data) of
		      	   continue ->
			   	    true;
		    	   E ->
			     Elements = exmpp_xml:remove_whitespaces_from_list(E),
			     lists:foreach(fun(X) -> exmpp_session:send_packet(Se,X) end,Elements)
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
	 	Binary = exmpp_stream:to_binary(Packet),
	 	%io:format("server ~p~n", [Binary]),
		gen_tcp:send(So,Binary),
	 	client_handler(So,Se,P);
	 stop ->
	      exmpp_xml:stop_parser(P);
	 _Catchall ->
	 	%io:format("client catchall ~p~n",[_Catchall]),
		client_handler(So,Se,P)
	 end.

reconnect(SL,J) ->
		S = SL(J),
		{_,ok} = update_session(J,S),
		monitor(process,S),
		S.
-include_lib("exmpp/include/exmpp_client.hrl").
server_handler(P,Id,J,T,SL) ->
	 receive
	 Record = #received_packet{raw_packet=Packet} ->
	 	case is_process_alive(P) of
		     true ->
		     	  %P ! exmpp_xml:set_attribute(Packet,<<"to">>,FullJID),
		     	  P ! Packet,
			  server_handler(P,Id,J,T,SL);
		     % comment when guard for off storing muc msgs and other stuff unsure if exmpp suports them though
		     false when Record#received_packet.packet_type == message orelse
		     	   	(Record#received_packet.packet_type == 'presence' andalso
				 (Packet#received_packet.type_attr == "subscribe" orelse
				  Packet#received_packet.type_attr == "subscribed")) ->
			  insert_message(T,Id,Packet),
			  server_handler(P,Id+1,J,T,SL);
		     _Catchall ->
		     	  server_handler(P,Id,J,T,SL)
		end;
	 P1 when is_pid(P1) ->
	 	 server_handler(P1,0,J,T,SL);
	 {'DOWN',_,_,_,_} ->
	      case is_process_alive(P) of
	      	     true ->
		   	P ! stop;
		     false ->
		   	true
	      end,
	      update_session(J,'session-in-restart'),
	      self() ! restart,
	      server_handler(P,Id,J,T,SL);
	 restart ->
	      io:format("~p server recon~n", [J]),
	      timer:sleep(?RECONNECT_TIME),
	      try reconnect(SL,J) of
	      	  S ->
		     io:format("~p session resurrected~n", [J]),
		     bnc_status(S),
	      	     server_handler(P,Id,J,T,SL)
	      catch
		  _Catchall ->
		     self() ! restart,
		     server_handler(P,Id,J,T,SL)
	      end;
	 stop ->
	      true;
	 _Catchall ->
	 	%io:format("server catchall ~p~n",[_Catchall]),
	 	server_handler(P,Id,J,T,SL)
	 end.

init_db() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(sessions,[{attributes,record_info(fields,sessions)}]).
find_session(J) ->
		  F = fun() -> mnesia:match_object(#sessions{jid=J,_='_'}) end,
		  {atomic,MR} = mnesia:transaction(F),
		  MR.
find_seen(T) ->
		  F = fun() -> mnesia:select(sessions,[{#sessions{seen='$1',_='_'},[{'<','$1',T}],['$_']}]) end,
		  {atomic,MR} = mnesia:transaction(F),
		  MR.
insert_session(J,P,S,T) ->
		 F = fun() -> mnesia:write(#sessions{jid=J,pid=P,session=S,seen=T}) end,
		 mnesia:transaction(F).
update_session(J,S) ->
		[MR] = find_session(J),
		F = fun() -> mnesia:write(MR#sessions{session=S}) end,
		mnesia:transaction(F).
delete_session(MR) ->
		F = fun() -> mnesia:delete_object(MR) end,
		mnesia:transaction(F).
update_seen(J,T) ->
		[MR] = find_session(J),
		F = fun() -> mnesia:write(MR#sessions{seen=T}) end,
		mnesia:transaction(F).
get_messages(T) ->
		try mnesia:dirty_select(T,[{'_',[],['$_']}]) of
		    Messages ->
		    	Messages
		catch
		    _ ->
		      []
		end.
insert_message(T,I,P) ->
		      catch mnesia:dirty_write(T,#messages{id=I,msg=P}).

dump() ->
	 Tables = lists:filter(fun(X) -> X /= schema andalso X /= sessions end,mnesia:system_info(tables)),
	 io:format("dumping ~p~n",[Tables]),
	 mnesia:dump_tables(Tables).
cleanup(T) ->
	 Time = get_time() - T,
	 Tables = find_seen(Time),
	 io:format("clearing ~p~n",[Tables]),
	 lists:foreach(fun(M) ->
	 		M#sessions.pid ! stop,
			exmpp_session:stop(M#sessions.session),
			delete_session(M),
			mnesia:clear_table(table_name(M#sessions.jid))
	 		end,Tables).
