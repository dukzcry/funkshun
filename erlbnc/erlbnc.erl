-author('A.V. Lukyanov <lomka@gero.in').

-module(erlbnc).
-export([start/0]).

-define(TIMEOUT,20000).
-define(LISTEN_PORT,5222).
-define(LIMIT_DOMAIN,<<"gero.in">>).
server(J,P) ->
	 % snapshot ver of exmpp is required for gtalk conn
	 S = exmpp_session:start({1,0}),
	 exmpp_session:auth_info(S,J,P),
	 [{Host, Port} | _] = exmpp_dns:get_c2s("gmail.com"),
	 {ok,_,_} = exmpp_session:connect_TCP(S,Host,Port,[{starttls,enabled}]),
	 {ok,_} = exmpp_session:login(S,"PLAIN"),
	 S.
 
start() ->
	exmpp:start(),
	init_db(),
	listen().

listen() ->
	 {ok,Socket} = gen_tcp:listen(?LISTEN_PORT,[binary,{packet,0},{active,false},{reuseaddr,true}]),
	 accept(Socket).
accept(LS) ->
	  case gen_tcp:accept(LS) of
	       {ok,Socket} ->
	        Pid = spawn(fun() -> client(Socket) end),
	  	gen_tcp:controlling_process(Socket,Pid),
		accept(LS);
	       _Catchall ->
	       accept(LS)
	  end.

getandparse(S,D) ->
       {ok,Data} = gen_tcp:recv(S,0,?TIMEOUT),
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
	 [_,Login,Password] = binary:split(list_to_binary(Challenge),[<<0>>],[global]),
	 ok = send(S,exmpp_server_sasl:success()),

	 ok = send(S,exmpp_stream:opening_reply(getandparse(S,1),random)),
	 ok = send(S,exmpp_stream:features([exmpp_server_binding:feature(),exmpp_server_session:feature()])),
	 Bind = getandparse(S,0),
	 BindWorkaround = Bind#xmlel{ns = ?NS_JABBER_CLIENT},
	 Resource = case exmpp_server_binding:wished_resource(BindWorkaround) of
	 	    	 undefined ->
	 		 	   "erlbnc";
	 		 R ->
	 		 	  R
	 	    end,
	 JID = exmpp_jid:make(binary_to_list(Login),binary_to_list(Domain),Resource),
	 {RemoteHandler,RemoteSession} = connect(JID,binary_to_list(Password)),
	 ok = send(S,exmpp_server_binding:bind(BindWorkaround,JID)),
	 Session = getandparse(S,0),
	 SessionWorkaround = Session#xmlel{ns = ?NS_JABBER_CLIENT},
	 % psi wants to set session even if we don't advertise this feature
	 ok = send(S,exmpp_server_session:establish(SessionWorkaround)),

	 P = exmpp_xml:start_parser([{root_depth,1}]),
	 % since root elm is lost
	 exmpp_xml:parse(P,"<stream:stream xmlns:stream='fake'>"),
	 RemoteHandler ! self(),
	 % quickly set off to be notified of roster people presences
	 exmpp_session:send_packet(RemoteSession,exmpp_presence:set_status(exmpp_presence:unavailable(),"")),
	 client_handler(S,RemoteSession,P).

bnc_status(S) ->
	   exmpp_session:send_packet(S,exmpp_presence:set_status(exmpp_presence:set_show(exmpp_presence:available(),'xa'),"erlbnc")).
% todo handle pings, stream ends
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
	 _Catchall ->
			server_handler(So,Se,P)
	 end.
-include_lib("exmpp/include/exmpp_client.hrl").
server_handler(S,P,J) ->
	 receive
	 % todo restart
	 stop ->
	      %io:format("server discon~n"),
	      exmpp_session:stop(S);
	 #received_packet{raw_packet=Packet} ->
		%P ! exmpp_xml:set_attribute(Packet, <<"to">>, J),
		P ! Packet,
	 	server_handler(S,P,J);
	 P1 when is_pid(P1) ->
	 	 server_handler(S,P1,J);
	 _Catchall ->
	 	server_handler(S,P,J)
	 end.

-record(sessions,{jid,pid,session}).

connect(J,P) ->
	Session = server(J,P),
	case find_session(J) of
	     [Record] ->
	     	exmpp_session:stop(Session),
		{Record#sessions.pid,Record#sessions.session};
	     [] ->
	     	Pid = spawn(fun() -> server_handler(Session,[],exmpp_jid:to_binary(J)) end),
		% todo kill proc on error
		ok = exmpp_session:set_controlling_process(Session,Pid),
	     	{_,ok} = insert_session(J,Pid,Session),
		{Pid,Session}
	end.

init_db() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(sessions,[{attributes,record_info(fields,sessions)}]).
find_session(J) ->
		  SR = #sessions{jid=J,_='_'},
		  F = fun() -> mnesia:match_object(SR) end,
		  {atomic,MR} = mnesia:transaction(F),
		  MR.
insert_session(J,P,S) ->
		SR = #sessions{jid=J,pid=P,session=S},
		 F = fun() -> mnesia:write(SR) end,
		 mnesia:transaction(F).
