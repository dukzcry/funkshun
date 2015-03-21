-module(eggbnc).

-author('A.V. Lukyanov <lomka@gero.in>').

-export([start/0,dump/0,cleanup/1,leave/1,kill/1,leave_rooms/1,get_all/1]).

% user config
-define(RECV_TIMEOUT,20000).
-define(RECONNECT_TIME,30000).
-define(LISTEN_ADDR,{127,0,0,1}).
-define(LISTEN_PORT,5222).
-define(LIMIT_DOMAIN,<<"example.com">>).
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
	 {ok,Socket} = gen_tcp:listen(?LISTEN_PORT,[binary,{packet,0},{active,false},{reuseaddr,true}
	 % comment to allow listening on all addresses
	 ,{ip,?LISTEN_ADDR}
	 ]),
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

-record(sessions,{jid,pid,seen,session}).
-record(messages,{id,stamp,msg}).
-record(rooms,{room,seen,barejid,session}).

get_time() ->
	   {_,Secs,_} = os:timestamp(),
	   Secs.

table_name(J) ->
	binary_to_atom(binary:replace(base64:encode(exmpp_jid:to_binary(J)),<<"/">>,<<"_">>,[global]),latin1). 
connect(J,P) ->
	TableName = table_name(J),
	ServerLambda = fun() -> server(J,P) end,
	Session = ServerLambda(),
	case find_session(TableName) of
	     [Record] ->
	     	exmpp_session:stop(Session),
		{_,ok} = update_seen(TableName,get_time()),
		{Record#sessions.pid,Record#sessions.session,TableName};
	     [] ->
	     	Pid = spawn(fun() -> monitor(process,Session), server_handler([],0,TableName,ServerLambda,Session,?RECONNECT_TIME,0) end),
		% todo kill proc on error
		ok = exmpp_session:set_controlling_process(Session,Pid),
	     	{_,ok} = insert_session(TableName,Pid,Session,get_time()),
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

% todo speak muc not groupchat
rejoin_(To,Pr) ->
	 bnc_status_(#xmlel{ns=?NS_JABBER_CLIENT,name='presence',attrs=[exmpp_xml:attribute(<<"to">>,To)]},Pr).
rejoin(S,Pr) ->
	     Rooms = find_rooms(S),
	     lists:foreach(fun(X) -> exmpp_session:send_packet(S,rejoin_(X#rooms.room,Pr)) end,Rooms).

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
	 % for clients like os x messages
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

	 case dirty_get_all(TableName) of
	      [] -> true;
	      M ->
	      	%io:format("off msgs ~p~n", M),
	      	lists:foreach(fun(X) -> send(S,add_delayed(X#messages.stamp,X#messages.msg)) end,lists:sort(M)),
		mnesia:clear_table(TableName), mnesia:dump_tables([TableName])
	 end,

	 P = exmpp_xml:start_parser([{root_depth,1}]),
	 % since root elm is lost
	 exmpp_xml:parse(P,"<stream:stream xmlns:stream='fake'>"),
	 RemoteHandler ! self(),
	 % quickly set off to be notified of roster people presences
	 exmpp_session:send_packet(RemoteSession,exmpp_presence:set_status(exmpp_presence:unavailable(),"")),
	 % todo resurrect prio from db, purpose?
	 rejoin(RemoteSession,0),
	 client_handler(S,RemoteSession,RemoteHandler,P,0,[Login,Domain]).

bnc_status_(T,P) ->
		exmpp_presence:set_priority(exmpp_presence:set_status(exmpp_presence:set_show(T,'xa'),"eggbouncer"),P).
bnc_status(S,P) ->
	   exmpp_session:send_packet(S,bnc_status_(exmpp_presence:available(),P)).

handle_packet(#xmlel{ns = NS} = Presence,Pr,S,BJ) when Presence#xmlel.name == 'presence' ->
    case exmpp_xml:get_element(Presence,NS,'priority') of
        undefined ->
	    case exmpp_stanza:get_type(Presence) of
	    	 <<"unavailable">> ->
		 	   case exmpp_xml:get_attribute(Presence,<<"to">>,[]) of
			   [] ->
			      % probably too much
			      bnc_status(S,Pr);
			   To ->
			      %io:format("effort to leave room ~p~n",[To]),
			      add_room(To,BJ,S),
			      %exmpp_session:send_packet(S,bnc_status_(exmpp_presence:set_type(Presence,available),Pr))
			      exmpp_session:send_packet(S,rejoin_(To,Pr))
			   end;
	    	 _ ->
		 	   exmpp_session:send_packet(S,Presence)
      	      end;
        Priority_El ->
	    case exmpp_xml:get_attribute(Presence,<<"to">>,[]) of
    	    [] ->
		self() ! case exmpp_xml:get_cdata_as_list(Priority_El) of
                "" -> 0;
                P  -> list_to_integer(P)
            	end;
	    _ ->
      	        true
	    end,
	    exmpp_session:send_packet(S,Presence)
    end;
handle_packet(Packet,_,S,_) ->
	exmpp_session:send_packet(S,Packet).
% todo catch pings, stream ends, spoof from/to for server changed resources
client_handler(So,Se,Pid,P,Pr,BJ) ->
	 inet:setopts(So,[{active,once}]),
	 receive
	 {tcp,So,Data} ->
		      case exmpp_xml:parse(P,Data) of
		      	   continue ->
			   	    true;
		    	   E ->
			     Elements = exmpp_xml:remove_whitespaces_from_list(E),
			     %io:format("client ~p~n", [Data]),
			     lists:foreach(fun(X) -> handle_packet(X,Pr,Se,BJ) end,Elements)
		      end,
		      client_handler(So,Se,Pid,P,Pr,BJ);
	 {tcp_closed,So} ->
	 		%io:format("client ~p discon~n", [So]),
			exmpp_xml:stop_parser(P),
			bnc_status(Se,Pr),
			% set away in case client not set unavail
			rejoin(Se,Pr);
	 {tcp_error,So,_Reason} ->
	 		%io:format("error client ~p sock ~p~n", [So,Reason]),
			exmpp_xml:stop_parser(P),
			bnc_status(Se,Pr),
			rejoin(Se,Pr);
	 Packet = #xmlel{} ->
	 	Binary = exmpp_stream:to_binary(Packet),
	 	%io:format("server ~p~n", [Binary]),
		gen_tcp:send(So,Binary),
	 	client_handler(So,Se,Pid,P,Pr,BJ);
	 NewPriority when is_integer(NewPriority) ->
	 	Pid ! NewPriority,
	 	client_handler(So,Se,Pid,P,NewPriority,BJ);
	 stop ->
	      exmpp_xml:stop_parser(P);
	 _Catchall ->
	 	%io:format("client catchall ~p~n",[_Catchall]),
		client_handler(So,Se,Pid,P,Pr,BJ)
	 end.

reconnect(SL,T) ->
		S = SL(),
		{_,ok} = update_session(T,S),
		monitor(process,S),
		S.
-include_lib("exmpp/include/exmpp_client.hrl").
server_handler(P,Id,T,SL,S,R,Pr) ->
	 receive
	 Record = #received_packet{raw_packet=Packet} ->
	 	case is_process_alive(P) of
		     true ->
		     	  %P ! exmpp_xml:set_attribute(Packet,<<"to">>,FullJID),
		     	  P ! Packet,
			  server_handler(P,Id,T,SL,S,R,Pr);
		     % comment when guard for off storing of other types of stanza
		     false when Record#received_packet.packet_type == message andalso
		     	   	 Record#received_packet.type_attr =/= "error" ->
			  case exmpp_message:get_body(Packet) of
			       undefined ->
			       		server_handler(P,Id,T,SL,S,R,Pr);
			       _ ->
					insert_message(T,Id,Packet),
					server_handler(P,Id+1,T,SL,S,R,Pr)
			  end;
		     false when	Record#received_packet.packet_type == 'presence' andalso
				 (Record#received_packet.type_attr == "subscribe" orelse
				  Record#received_packet.type_attr == "subscribed") ->
			  insert_message(T,Id,Packet),
			  server_handler(P,Id+1,T,SL,S,R,Pr);
		     % uncomment to autorejoin on kick
		     %false when Record#received_packet.packet_type == 'presence' andalso
		     %	   Record#received_packet.type_attr == "unavailable" ->
		     %	   	case exmpp_xml:get_attribute(exmpp_xml:get_element(exmpp_xml:get_element(Packet,'x'),'status'),<<"code">>,[]) of
		     %	     	     <<"307">> ->
		     %		     	       From = exmpp_xml:get_attribute(Packet,<<"from">>,[]),
		     %			       exmpp_session:send_packet(S,rejoin_(From,Pr));
		     %		     _ ->
		     %			       true
		     %		end,
		     %		server_handler(P,Id,T,SL,S,R,Pr);
		     _Catchall ->
		     	  %io:format("server packet catch all ~p~n",[Record]),
		     	  server_handler(P,Id,T,SL,S,R,Pr)
		end;
	 P1 when is_pid(P1) ->
	 	 server_handler(P1,0,T,SL,S,R,Pr);
	 {'DOWN',_,_,_,_} ->
	      case is_process_alive(P) of
	      	     true ->
		   	P ! stop;
		     false ->
		   	true
	      end,
	      update_session(T,'session-in-restart'),
	      self() ! restart,
	      server_handler(P,Id,T,SL,S,R,Pr);
	 restart ->
	      io:format("~p server recon~n", [T]),
	      timer:sleep(R),
	      try reconnect(SL,T) of
	      	  NewS ->
		     io:format("~p session resurrected~n", [T]),
		     bnc_status(NewS,Pr),
		     rejoin(NewS,Pr),
	      	     server_handler(P,Id,T,SL,NewS,?RECONNECT_TIME,Pr)
	      % probably too much
	      catch
		  _Catchall ->
		     self() ! restart,
		     server_handler(P,Id,T,SL,S,R*2,Pr)
	      end;
	 NewPriority when is_integer(NewPriority) ->
	 	     server_handler(P,Id,T,SL,S,R,NewPriority);
	 stop ->
	      true;
	 _Catchall ->
	 	%io:format("server catchall ~p~n",[_Catchall]),
	 	server_handler(P,Id,T,SL,S,R,Pr)
	 end.

init_db() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(sessions,[{attributes,record_info(fields,sessions)}]),
	mnesia:create_table(rooms,[{attributes,record_info(fields,rooms)}]).
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
find_rooms(S) ->
	      F = fun() -> mnesia:match_object(#rooms{session=S,_='_'}) end,
	      {atomic,MR} = mnesia:transaction(F),
	      MR.
add_room(R,BJ,S) ->
		  F = fun() ->
		  MR = mnesia:match_object(#rooms{room=R,barejid=BJ,_='_'}),
		  case MR of
		  [] ->
		     mnesia:write(#rooms{room=R,barejid=BJ,session=S,seen=get_time()});
		  [Room] ->
		     mnesia:write(Room#rooms{session=S,seen=get_time()})
		  end
		  end,
		  mnesia:transaction(F).
leave_rooms_(R) ->
	      	 Packet = exmpp_presence:unavailable(),
	      	 lists:foreach(fun(X) -> exmpp_session:send_packet(X#rooms.session,exmpp_xml:set_attribute(Packet,<<"to">>,X#rooms.room)),
		 		      F = fun() -> mnesia:delete_object(X) end,
				      mnesia:transaction(F)
				      end,R).
leave_rooms(T) when is_atom(T) ->
		[Session] = find_session(T),
		Rooms = find_rooms(Session#sessions.session),
		leave_rooms_(Rooms);
leave_rooms(R) ->
	       leave_rooms_(R).
find_stale(T) ->
		  F = fun() -> mnesia:select(rooms,[{#rooms{seen='$1',_='_'},[{'<','$1',T}],['$_']}]) end,
		  {atomic,MR} = mnesia:transaction(F),
		  MR.

dump() ->
	 Tables = lists:filter(fun(X) -> mnesia:table_info(X,record_name) == messages end,mnesia:system_info(tables)),
	 io:format("dumping ~p~n",[Tables]),
	 mnesia:dump_tables(Tables).
cleanup(T) ->
	 Tables = find_seen(get_time() - T),
	 io:format("clearing ~p~n",[Tables]),
	 lists:foreach(fun(X) -> kill_session(X) end,Tables).
leave(T) ->
	 StaleRooms = find_stale(get_time() - T),
	 io:format("leaving ~p~n",[StaleRooms]),
	 leave_rooms_(StaleRooms).
kill(J) ->
	[MR] = find_session(J),
	kill_session(MR).
