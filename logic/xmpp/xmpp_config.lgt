:- object(xmppConfig, imports([options, xmppJid])).

	:- public([xmpp_context/1, xmpp_context/2]).
	
	:- uses(list, [memberchk/2]).
	
	%:- meta_predicate(config(*, *)).

	xmpp_context(C) :-
		xmpp_context(C, []).
 

	xmpp_context(C, UserOptions) :- 
		weak_check_options(UserOptions),
        ^^merge_options(UserOptions, Options),
		
		context_jid(Jid, Options),
		context_proxy(Proxy, Options),
		context_hostport(HostPort, Options),
		context_tls(Tls, Options),
		context_password(Password, Options), 
		
		C = xmppContext{ 	jid: Jid,   
							password: Password,
							proxy: Proxy, 
							host: HostPort, 
							tls: Tls, 
							xmpp_server_live: false
		}.

	weak_check_options([]) :-!.
	weak_check_options([password(_) | Tail]) :-
		weak_check_options(Tail) ,!.

	weak_check_options([Option | Tail]) :-
		^^check_option(Option),
		weak_check_options(Tail).

	context_password(Password, Options) :-
		memberchk(password(Password), Options),
		( Password = none -> 
			throw(xmpp_error(user_password_required, _))
			;
			true
		).
 
	context_tls(Tls, Options) :-
		memberchk(tls(Tls), Options).

	context_hostport(HostPort, Options) :- 
		domain(Domain, Options),
		memberchk(port(Port), Options),
		HostPort = Domain:Port.
		%format(atom(HostPort), '~w:~w', [Domain, Port]).
	
	context_jid(JIDA, Options) :-
		domain(Domain, Options),
		memberchk(account(User), Options),
		::jid_atom(jid(User, Domain, none), JIDA).

	context_proxy(Proxy, Options) :-
		memberchk(proxy(Proxy), Options).

	domain(Domain, Options) :-
		memberchk(domain(D), Options),
		memberchk(host(Host), Options),
		(D = none ->
			Domain = Host
		;
			Domain = D
		).
	
	%%%%%%%%%%%%% defaults for every option needed wiexept password %%%%%%%%%%%%%%%%%%

	default_option(port(5222)).
	default_option(domain(none)).
	default_option(host('localhost')).
	default_option(tls(none)).
	default_option(account('chef')).
	default_option(password(none)).
	default_option(proxy(none)).

	% Load config default data from file (not used at the moment)

	load_config(File, _Options) :-
		logtalk_load(File).


:- end_object.