:- object(xmppSecurity, imports([options])).
	:- public([ tls_connect/4, sasl_auth_xml/4]).
	
	:- meta_predicate(ssl:ssl_context(*, *, *)).
	:- meta_predicate(ssl:ssl_negotiate(*, *, *, *, *)).

	:- uses(list, [memberchk/2]).

	:- use_module(base64, [base64/2]).
	:- use_module(ssl, [ssl_context/3,
						ssl_negotiate/5]).

	default_option(no_cert_verify). % TODO support pinned certs
	

	tls_connect(Host, Client, TlsPair, UserOptions) :-
		^^merge_options(UserOptions, Options),
		( memberchk(no_cert_verify, Options)
			-> CertHook = cert_accept_any
			;  CertHook = cert_accept_any   
		),
		xmppXML::xml_out(starttls, Client.raw_streams),
		xmppXML::xml_in(Client, [//proceed->needed(no_starttls)]),
		ssl_context(client, Ssl, [ host(Host), require_crl(false), close_parent(true),
								cert_verify_hook(CertHook)
								%,min_protocol_version(tlsv1_1)
					]),
		stream_pair(Client.raw_streams, Read, Write),
		ssl_negotiate(Ssl, Read, Write, TlsRead, TlsWrite),
		!,
		stream_pair(TlsPair, TlsRead, TlsWrite).


	sasl_prep(StrIn, StrIn). %  TODO: Implement SASLprep https://tools.ietf.org/html/rfc4013

	sasl_plain_userpwd(User, Pwd, Base64) :-
		sasl_prep(User, User1),
		sasl_prep(Pwd, Pwd1),
		format(atom(PlainSASL),"\x00~w\x00~w",[User1, Pwd1]),
		base64(PlainSASL, Base64).

	sasl_auth_xml(plain, User, Pwd, XML) :-
		sasl_plain_userpwd(User, Pwd, Base64),
		format(string(XML),
			"<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN'>~w</auth>",
			[Base64]).

:- end_object.
