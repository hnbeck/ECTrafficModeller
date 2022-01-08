:- object(xmppXML, 
		imports([options]), 
		implements([xmppXMLProtocol])).

	:- meta_predicate(call_handler(*, *, *)).
	:- meta_predicate(sgml:on_error(*, *, *)).
	:- meta_predicate(sgml:load_xml(*, *, *)).
		
	:- uses(list, [memberchk/2]).	

	:- use_module(pcre, [ re_match/2]).
	:- use_module(library(pprint), [print_term/2]).
	
	:- use_module(sgml, [new_sgml_parser/2,
							set_sgml_parser/2,
							sgml_parse/2,
							free_sgml_parser/1,
							load_xml/3]).
	:- use_module(xpath, [xpath_chk/3]).
	:- use_module(apply, [maplist/3, 
							exclude/3]).
	:- use_module(strings, [indent_lines/3]).	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%% output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	xml_out(stream(From, To, _Id), Pair) :-
		format(atom(XML),
				"<?xml version='1.0'?><stream:stream from='~w' to='~w' version='1.0' xml:lang='de-DE' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>",
			[From, To]),
		xml_out_(Pair, XML).

	xml_out(starttls, Pair) :-
		format(atom(XML),"<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls' />",[]),
		xml_out_(Pair, XML).

	xml_out(sasl_auth(plain, User, Pwd), Pair) :-
		xmppSecurity::sasl_auth_xml(plain, User, Pwd, XML),
		xml_out_(Pair, XML).

	xml_out(raw(Pair), XML) :-
		xml_out_(Pair, XML).

	xml_out_(Pair, XML) :-
		write(Pair,XML),
		flush_output(Pair),
		logtalk::print_message(debug, xmpp,'Outgoing:~n~w~n'+[XML]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	default_option(context(in_stream)).

	xml_in(Client, Handlers) :-
		xml_in(Client, Handlers, _Results, []).

	xml_in(Client, Handlers, Results) :-
		xml_in(Client, Handlers, Results, []).

	xml_in(Client, Handlers, Results, UserOptions) :-
		^^merge_options(UserOptions, Options),
		memberchk(context(Context), Options),
		Read = Client.raw_streams,
		xml_in_(Client, Context, Read, Handlers, Results, _DOM).

	xml_in_(Client, Context, Read, Handlers, Results, DOM) :-
		xml_source(Client, Context, Read, Read1),
		new_sgml_parser(Parser, []),
		set_sgml_parser(Parser, dialect(xml)),
		set_sgml_parser(Parser, space(sgml)),
		sgml_parse(Parser,
					[ source(Read1),
					parse(content),
					document(DOM)
					%call(error, on_error)
				]),
		nb_setval(xmpp_stream_in, xmpp{result:[]}),
		call_handlers( DOM, Handlers),
		nb_getval(xmpp_stream_in, XMPPStreamDict),
		Results = XMPPStreamDict.result,
		free_sgml_parser(Parser).


	on_error(Severity, Message, _Parser) :-
		end_tag('stream:stream', Message),
		!,
		logtalk::print_message(debug, xmpp,'received error: ~w ~w~n'+[Severity, Message]).

	on_error(Severity, Message, _Parser) :-
		logtalk::print_message(debug, xmpp,'received error: ~w ~w~n',[Severity, Message]).

	end_tag(Tag, Msg) :-
		format(string(Expected),'Ignored end-tag for "~w" which is not open',[Tag]),
		sub_string(Msg, _, _, _, Expected).


	call_handlers( DOM, Handlers) :-
		maplist(call_handler_( DOM), Handlers, Responses1),
		exclude([E]>>(E=='$xpath-no-match$'), Responses1, Responses),
		stream_in_set(result,[result=Responses]).

	call_handler_( DOM, Handler, Response) :-
		logtalk::print_message(debug, xmpp,'Handler : ~w'+ [Handler]),
		with_output_to(string(DOM1),
		print_term(DOM, [output(current_output), left_margin(3)])),
		indent_lines("   ", DOM1, DOM2),
		logtalk::print_message(debug, xmpp,'Handler dom in : ~w'+[DOM2]),
		call_handler( DOM, Handler, Response),
		logtalk::print_message(debug, xmpp,'Handler result: ~w'+[Response]).

	call_handler( DOM, XPath->needed(Response), '$handler-match$') :-
		!,  % only one 'needed' handler
		(  xpath_chk(DOM, XPath, _Content)
			-> true
			;  throw(xmpp_error(handler_no_match, Response))
		).

	call_handler( DOM, XPath->throw(Error), Response) :-
		!,  % only one 'throw' handler
		(  xpath_chk(DOM, XPath,_Content)
			-> throw(Error)
			;  Response = na
		).

	call_handler( DOM, XPath->call(Goal), Response) :-
		!,  % only one 'call' handler
		(  xpath_chk(DOM, XPath, Content)
			-> call(Goal, DOM, Content, Response)
			;  Response = na
		).


	stream_in_set(-Attr,Attrs) :-
		!, ignore(stream_in_set(Attr, Attrs)).

	stream_in_set(Attr,Attrs) :-
		memberchk(Attr=Val,Attrs),
		nb_getval(xmpp_stream_in,S),
		put_dict(Attr,S,Val,S1),
		nb_setval(xmpp_stream_in,S1),
		format('stream: ~w',[S1]).


	%%%%%%%%%%%%%%%%%

	xml_source(C, Type, StreamIn, StreamOut) :-
		% Set timeout to infinite  if we are just waiting
		% for stanzas or to a few seconds otherwise
		(  Type == wait_for_stanza
			-> set_stream(StreamIn, timeout(infinite))
			;  set_stream(StreamIn, timeout(20))
		),
		% Search for stream:features end tag if we are
		% negotiating a stream, otherwise search for
		% a complete element
		logtalk::print_message(debug, xmpp,'Source Type ~w'+[Type]),
		(  Type == stream_features
			-> peek_tag(C, StreamIn, "\\/stream:features>|\\/stream:stream>", Length, XML)
			;  peek_tag(C, StreamIn, "^<(\\S+)[^>]*>.*</\\1\s*>|<[^<]+/\\s*>", Length, XML)  % closed xml element
		),
		% This is to move the stream position to the proper place
		% seek/4 didn't work at first try, so I just decided to do
		% it in this less efficient way
		
		read_string(StreamIn, Length, _),
		logtalk::print_message(debug, xmpp,'Incoming:~n~w'+[XML]),
		% Produce a stream which reaches the end when the XML snippet ends
		open_string(XML, StreamOut).
	 
	 
	 % peek_tag(C,Stream,Regex,Len,XML) :-
	 %    debugging(xmpp(xml),false),
	 %    between(1,4096,Len),
	 %    peek_string(Stream,Len,XML),
	 %    re_match(Regex,XML),
	 %    !.  % Once a match is found, don't try larger lengths
	 
	 % TODO: this is ugly see if it can be fixed
	 peek_tag(C, Stream, Regex, Len, XML) :-
		%debugging(xmpp(xml)),
		nb_setval(xml0, XML),
		between(1, 4096, Len),
		nb_getval(xml0, XML0),
		catch(peek_string(Stream, Len, XML), E,
		   ( logtalk::print_message(debug, xmpp,"XML received on exception: '~w'"+[XML0]),
			 (  XML0 == " "
				-> ( XML = XML0, xmpp_server_status(C, true) )
				;  throw(E)
		 	)
		   )
		),
		(  XML \== " "
			-> ( nb_setval(xml0, XML), re_match(Regex, XML) )
			;  true
		),!.  % Once a match is found, don't try larger lengths
	 
	 
	 xmpp_server_status(C, true) :-
		b_set_dict(xmpp_server_live, C, true),
		logtalk::print_message(debug, xmpp,'XMPP Server is live'+[]).
	 
	 xmpp_server_status(C, false) :-
		b_set_dict(xmpp_server_live, C, false),
		logtalk::print_message(debug, xmpp,'XMPP Server is dead'+[]).
	
	 
:- end_object.
							 