:- object(xmppCore).
	:- public([xmpp_connect/2,
				xmpp_close/1, 
				xmpp_send_message/4,
				xmpp_send_message/3]).

	:- use_module(library(option), [option/2, option/3]).
	:- use_module(socket, [tcp_connect/3,
							tcp_getopt/2]).
	:- use_module(gensym, [gensym/2]).
		
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%% main API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	xmpp_connect(Context, Client) :-
		c(not_connected, Context) = S0, 
		handle_event(resolve_name, S0, S1, _),
		handle_event(connect_transport, S1, S2, _),
		S2 = c(_, Context2),
		logtalk::print_message(debug, xmpp, 'client is ~w'+[Context2]),
		( _{tls:none} :< Context2 ->
		   	handle_event(plain_transport, S2, S3, _),
			logtalk::print_message(debug, xmpp, 'plain transport before'+[])
		   ;
		   	handle_event(secure_transport, S2, S3, _)
		),
		handle_event(authenticate, S3, S4,_),
		handle_event(bind_resources, S4, Client, _).
	 
	xmpp_close(c(S, Client)) :-
		S0 = c(S, Client),
		handle_event(close, S0, _S1, _Err).
	 
	xmpp_send_message(c(_, C), To, Msg, Options) :-
		(  option(subject(Subj), Options)
			-> format(string(Content),"<subject>~w</subject><body>~w</body>",[Subj,Msg])
			;  format(string(Content),"<body>~w</body>",[Msg])
		),
		ignore(option(id(Id), Options)),
		stanza_out(message(chat, To), Content, c(_,C), Id).
 
	% xmpp_send_message(To, Msg, Options) :-
	% 	xmpp_client(C),
	% 	xmpp_connect(C, C1),
	% 	xmpp_send_message(C1, To, Msg, Options),
	% 	xmpp_close(C1).


	 :- multifile socket:proxy_for_url/3.           % +URL, +Host, -ProxyList
	 
	 socket:proxy_for_url(_Url, _Host, Proxy) :-
		nb_getval(xmpp_proxy, Proxy),
		logtalk::print_message(debug, xmpp, 'Using proxy: ~w'+[Proxy]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%% state machine %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	handle_event(Event, c(FromState, C1), c(ToState, C2), Error) :-
		event_state(Event, FromState, ToState),
		!,  % Display unable to handle event error only if event doesn't exist
		get_time(T0),
		xmpp_client(c(FromState, C1), c(ToState, C2), Error),
		get_time(T1),
		T is T1 - T0,
		(  Error \== none
			-> print_message(error,Error)
			;  logtalk::print_message(debug, xmpp, 'Reached state ~w in ~2f secs.'+[ToState,T])
		).
		 
	handle_event(Event,_,_,_) :-
		print_message(error, xmpp_error(unable_to_handle_event(Event))).

	% Stream setup
	event_state(resolve_name, not_connected, name_resolved).
	event_state(connect_transport, name_resolved, transport_ready).
	event_state(plain_transport, transport_ready, transport_open).
	%event_state(secure_transport, transport_ready, transport_secured).
	event_state(authenticate, transport_secured, authenticated).
	event_state(authenticate, transport_open, authenticated).
	event_state(bind_resources, authenticated, resources_bound).

	% Receive and Send stanzas
	event_state(rx_stanza, authenticated, authenticated).
	event_state(tx_stanza, authenticated, authenticated).

	event_state(close, _, not_connected).


	%%%%%%%%%%% state transformation %%%%
	% DNS resolve
	% ===================
	xmpp_client(c(not_connected, C1), c(name_resolved, C1), none) :-
		!,    % One state at a time
		true. % TODO: maybe support SRV records like the RFC says

	
	% TCP connection
	% ===================
	xmpp_client(c(_S1, C1), c(transport_ready, C), Error) :-
		!,    % One state at a time
		(  C1.proxy == none -> ByPassProxy = true;  ByPassProxy = false ),
		get_dict(host, C1, Host:Port),
		tcp_connect(Host:Port, Pair, [ bypass_proxy(ByPassProxy) ]),
		stream_pair(Pair, Read, _Write),
		tcp_getopt(Read, file_no(ReadFd)),
		put_dict(_{ read_stream_fd:   ReadFd,
					xmpp_server_live: true,
					raw_streams: Pair }, C1, C),
		Error = none,
		logtalk::print_message(debug, xmpp, 'TCP raw streams ready'+[]).
	
	
	% TLS negotiation
	% ===================
	xmpp_client(c(_S1, C1), c(transport_open, C1), Error) :- !,
		logtalk::print_message(debug, xmpp,'State in plain ~w, Context ~w'+[_S1, C1]),
		get_dict(jid, C1, Jid),
		get_dict(raw_streams, C1, Pair),
		Error = none,
		logtalk::print_message(debug, xmpp,'bis hier alles gut ~w'+[_S1]),
		send_stream_request(Jid, Pair),
		logtalk::print_message(debug, xmpp,'Plain raw streams ready'+[]).
	
	xmpp_client(c(_S1, C1), c(transport_secured, C), Error) :-
		!,    % One state at a time
		C1 = xmppContext{ jid: Jid,   password: Password,
							proxy: Proxy, host: Host:Port, tls: tls(no_cert_verify),
				raw_streams: Pair, read_stream_fd: Fd, xmpp_server_live: L
				},
		C = xmppContext{ jid: Jid,   password: Password,
		 					proxy: Proxy, host: Host:Port, tls: tls(no_cert_verify),
		 		raw_streams: TlsPair, read_stream_fd: Fd, xmpp_server_live: L
		 		},
		send_stream_request(Jid, Pair),
		% Response
		xmppXML::xml_in(C1, [//starttls/required->needed(starttls)],
			_Result,[context(stream_features)]),
		Error = none,
		xmppSecurity::tls_connect(Host, C1, TlsPair,[no_cert_verify, read_fd(Fd)]),  % TODO support pinned certs
		logtalk::print_message(debug, xmpp, 'TLS raw streams ready'+[]).
	
	
	
	% SASL authentication
	% ===================
	
	xmpp_client(c(_S1, C1), c(authenticated, C1), Error) :-
		!,    % One state at a time
		% Plain mechanism offered
		send_stream_request(C1.jid, C1.raw_streams),
		xmppXML::xml_in(C1, 
						[//mechanisms/mechanism(content=['PLAIN'])->needed(no_plain_sasl_offered)],
						_,
						[context(stream_features)]),
		% Send plain auth, accept success
		xmppConfig::jid_atom(jid(User,_Domain,_Resource), C1.jid),
		xmppXML::xml_out(sasl_auth(plain, User, C1.password), C1.raw_streams),
		xmppXML::xml_in(C1, [//success->needed(no_authentication_success)]),
		Error = none.
	
	
	% Bind resources
	% ===================
	xmpp_client(c(_S1, C1), c(resources_bound, C), Error) :-
		!,    % One state at a time
	
		% Receive new stream
		send_stream_request(C1.jid, C1.raw_streams),
		xmppXML::xml_in(C1,[  //'stream:stream'( @id=Id)->needed(not_authorized),
								//'stream:features'/bind  ->needed(no_bind_feature)
							],
							_, 
							[context(stream_features)]),
		stanza_out(bind, C1, SId),
		stanza_reply(iq(result, SId), C1, bind(Jid)),
		xmppConfig::jid_atom(Jid1, Jid),
		put_dict(_{ stream_id: Id,
					jid:       Jid1
			}, C1, C),
		Error=none.
	
	% Close
	% ===================
	xmpp_client(c(_S1,C1), c(not_connected,C1), Error) :-
		Error=none,
		close(C1.raw_streams). % The ssl layer will close the underlying TCP streams
 

	%%%%%%%%%%%%%%%%%%%%%%%%%%% pure communication %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	send_stream_request(JidAtom, Pair) :-
		%Request
		xmppConfig::jid_atom(Jid, JidAtom),
		Jid = jid(_User, Domain, _Res),
		xmppXML::xml_out(stream(JidAtom, Domain, _Id), Pair).


	stanza_out(iq, set(Content), C, Id) :-
		gensym(iqset, Id),
		format(atom(XML),
			"<iq xmlns='jabber:client' type='set' id='~w'>~w</iq>", [Id, Content]),
		xmppXML::xml_out(raw(C.raw_streams), XML).
		
		%      message
	stanza_out(message(Type, To), Content, c(_,C), Id) :-
		gensym(msg, Id),
		xmppConfig::jid_atom(C.jid, From),
		format(atom(XML),
			"<message from='~w' id='~w' to='~w' type='~w' xml:lang='en'>
				~w
			</message>",
			[From,Id,To,Type, Content]),
		xmppXML::xml_out(raw(C.raw_streams), XML).
		
		%      Resource bind
	stanza_out(bind, C, Id) :-
		Resource = 'cGx4bXBwCg',
		%Resource = 'gajim.HBJM4J6K',
		format(string(Content),
			"<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>~w</resource></bind>",[Resource]),
		stanza_out(iq, set(Content), C, Id).


	stanza_reply(iq(result, Id), C, bind(Jid)) :-
		xmppXML::xml_in(C, [  //iq(@type=result,@id=Id)  ->needed(stanza_wrong_id),
						//bind/jid(content=[Jid])  ->needed(no_bind_reply_with_jid)
					 ]).
:- end_object.