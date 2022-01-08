:- module(xmpp_client,
          [  xmpp_client/1,
	         xmpp_client/2,
	         xmpp_connect/2,
            xmpp_send_message/3,
            xmpp_send_message/4,
	         xmpp_close/1 
	  ]
       ).

:- use_module(library(xpath)).
:- use_module(library(strings)).
:- use_module(library(socket)).
:- use_module(gpg).

:- debug(xmpp(xml)).
:- debug(xmpp(handler)).
:- debug(xmpp).

% Sample run:
% ?- xmpp_client([encrypted_config(false),config('/tmp/xmpp_conf')],C),
 %   xmpp_connect(C,C1),
  %  xmpp_send_message(C1,'someuser@localhost',"hi a test",[]),
 %   xmpp_close(C1).

%
% default config is in $HOME/.config/plxmpp/config.pl (encrypted with gpg)
%

%                              /**************
%                               *   Client   *
%                               **************/
% :- dynamic config/2,cmdserver_is_listening/1.

 xmpp_client(Client) :-
    xmpp_client([config(default)],Client).

 xmpp_client(Options,Client) :-
    option(config(File), Options),
    !,
    load_config(File,Options),
    (  config(default_account,Acct)
    -> true
    ;  throw(xmpp_error(no_default_in_config,File))
    ),
    findall(O,config(Acct,O),Options1),
    xmpp_client(Options1, Client).

 xmpp_client(Options,Client) :-
    option(config(File,Acct), Options),
    !,
    load_config(File,Options),
    findall(O,config(Acct,O),Options1),
    xmpp_client(Options1, Client).

 xmpp_client(Options,c(not_connected,Client)) :-
    (  option(jid(Jid1),Options)
    -> true
    ;  throw(xmpp_error(user_jid_required,_))
    ),
    (  option(password(PassSpec),Options)
    -> true
    ;  throw(xmpp_error(user_password_required,_))
    ),
    option(host(HostPort1),Options,none),
    option(proxy(Proxy1),Options,none),
    option(tls(Tls1),Options,none),
    jid_atom(Jid,Jid1),
    spec_hostport(Jid,HostPort1, HostPort),
    spec_password(PassSpec, Password),
    spec_proxy(Proxy1,Proxy),
    spec_tls(tls(Tls1),Tls),
    Client = xmppclient{ jid: Jid,   password: Password,
                         proxy: Proxy, host: HostPort, tls: Tls, xmpp_server_live: false
 		      }.


 spec_hostport(jid(_,Domain,_),none,Domain:5222) :- !.
 spec_hostport(_Jid,Host:Port,Host:Port) :- !.
 spec_hostport(_Jid,Host,Host:5222) :- !.

 spec_password(plain(Password),Password) :- !.
 spec_password(env(Var),Password) :- getenv(Var, Password), !.

 spec_proxy(none, none) :- nb_setval(xmpp_proxy,direct), !.
 spec_proxy(socks(Host:Port), socks(Host,Port)) :- nb_setval(xmpp_proxy,socks(Host,Port)), !.

 spec_tls(tls(none),tls(none)) :- !.
 % TODO: Support CA roots and verification
 spec_tls(tls(no_cert_verify),tls(no_cert_verify)) :- !.
 spec_tls(tls(pin(Goal)),tls(pin(Goal))) :- !.


                            /****************
                             *    Connect   *
                             ****************/


 xmpp_connect(Client1, Client) :-
    S0 = Client1,
    c(_, Dict) = S0, 
    handle_event(resolve_name,S0,S1,_),
    handle_event(connect_transport,S1,S2,_),
    debug(xmpp, "client is ~w ~n", [Dict]),
    (  _{tls:tls(none)} :< Dict ->
       handle_event(plain_transport,S2,S3,_),
       debug(xmpp, "plain transport ~n", [])
       ;
       handle_event(secure_transport,S2,S3,_)
    ),
    handle_event(authenticate,S3,S4,_),
    handle_event(bind_resources,S4,Client,_).

 xmpp_close(c(S,Client)) :-
    S0 = c(S,Client),
    handle_event(close,S0,_S1,_Err).


 :- multifile socket:proxy_for_url/3.           % +URL, +Host, -ProxyList
 socket:proxy_for_url(_Url,_Host,Proxy) :-
    nb_getval(xmpp_proxy,Proxy),
    debug(xmpp,"Using proxy: ~w",[Proxy]).



                            /****************
                             *   Messages   *
                             ****************/

 xmpp_send_message(c(_,C),To,Msg,Options) :-
    (  option(subject(Subj),Options)
    -> format(string(Content),"<subject>~w</subject><body>~w</body>",[Subj,Msg])
    ;  format(string(Content),"<body>~w</body>",[Msg])
    ),
    ignore(option(id(Id),Options)),
    stanza_out(message(chat,To),Content,c(_,C),Id).

 xmpp_send_message(To,Msg,Options) :-
    xmpp_client(C),
    xmpp_connect(C,C1),
    xmpp_send_message(C1,To,Msg,Options),
    xmpp_close(C1).



                      /****************************
                       *   Local command server   *
                       ****************************/
% The local command server uses unix sockets to listen to commands
% from the local machine, it also listens to the xmpp connection stream
% to handle xmpp commands.
%
% xmpp_event_loop(c(State,C),Options) :
%    cmdserver_listening(CmdStream),
%    respond_to_streams(CmdStream-read_local_command,C-handle_xmpp_in,LoopCmd),
%    LoopCmd \== exit,
%    xmpp_event_loop(c(State,C),Options).

% xmpp_event_loop(c(State,C),Options) :-
%    debug(xmpp,"Exiting event loop",[]).

% respond_to_streams(CmdStream-CmdGoal,C-XMPPGoal,LoopCmd) :-
%    stream_pair(CmdStream,CmdRead,_),
%    wait_for_input([CmdRead,C.read_stream_fd],[RdyStream],infinite),
%    (  RdyStream == CmdRead
%    -> ignore(call(CmdGoal,C,RdyStream,LoopCmd))
%    ;  ignore(call(XMPPGoal,C,C.raw_streams,LoopCmd))
%    ).

% % read_local_command(C,SrvSocket,LoopCmd) :-
% %    tcp_accept(SrvSocket,ClientSocket,_Afunix),
% %    setup_call_cleanup(
% %       tcp_open_socket(ClientSocket,CmdClientPair),
% %       (  read_term(CmdClientPair,Term,[]),
% %          debug(xmpp,"Local command received: ~w",[Term]),
% % 	 handle_command(C,Term,LoopCmd)
% %       ),
% %       close(CmdClientPair)
% %    ).

% handle_command(C,Term,continue) :-
%    format('handling command: ~w~n',[Term]).

% handle_xmpp_in(C,Stream,continue) :-
%    xml_in_(C,in_stream,Stream,[],_Results,DOM),
%    write(DOM).

% cmdserver_socket(S) :-  atomic_list_concat(['/tmp/plxmpp-listener',x],S).

% cmdserver_listening(CmdPair) :-
%    cmdserver_is_listening(CmdPair),
%    !.

% cmdserver_listening(CmdPair) :-
%    \+ cmdserver_is_listening(_),
%    !,
%    cmdserver_socket(UnixSocketFile),
%    unix_domain_socket(UnixSocket),
%    tcp_bind(UnixSocket,UnixSocketFile),
%    tcp_listen(UnixSocket,3),
%    tcp_open_socket(UnixSocket,CmdPair),
%    assert(cmdserver_is_listening(CmdPair)),
%    at_halt(stop_cmdserver).

% % send_command(Cmd) :-
% %    cmdserver_socket(UnixSocketFile),
% %    unix_domain_socket(UnixSocket),
% %    tcp_connect(UnixSocket,UnixSocketFile),
% %    tcp_open_socket(UnixSocket,CmdPair),
% %    writeln(CmdPair,Cmd),
% %    flush_output(CmdPair).

% % stop_cmdserver :-
% %    cmdserver_is_listening(CmdPair),
% %    cmdserver_socket(UnixSocketFile),
% %    close(CmdPair),
% %    delete_file(UnixSocketFile),
% %    retractall(cmdserver_is_listening(_)).


                       /**************************
                        *   XMPP State machine   *
                        **************************/

 handle_event(Event,c(FromState,C1),c(ToState,C2),Error) :-
    event_state(Event,FromState,ToState),
    !,  % Display unable to handle event error only if event doesn't exist
    get_time(T0),
    xmpp_client(c(FromState,C1),c(ToState,C2),Error),
    get_time(T1),
    T is T1 - T0,
    (  Error \== none
    -> print_message(error,Error)
    ;  debug(xmpp,"Reached state ~w in ~2f secs.",[ToState,T])
    ).

 handle_event(Event,_,_,_) :-
    print_message(error,xmpp_error(unable_to_handle_event(Event))).

% % Stream setup
 event_state(resolve_name,not_connected,name_resolved).
 event_state(connect_transport,name_resolved,transport_ready).
 event_state(plain_transport, transport_ready, transport_open).
 event_state(secure_transport,transport_ready,transport_secured).
 event_state(authenticate,transport_secured,authenticated).
 event_state(authenticate,transport_open,authenticated).
 event_state(bind_resources,authenticated,resources_bound).

% % Receive and Send stanzas
 event_state(rx_stanza,authenticated,authenticated).
 event_state(tx_stanza,authenticated,authenticated).

 event_state(close,_,not_connected).

% % DNS resolve
% % ===================
 xmpp_client(c(not_connected,C1),c(name_resolved,C1),none) :-
    !,    % One state at a time
    true. % TODO: maybe support SRV records like the RFC says



% % TCP connection
% % ===================
 xmpp_client(c(_S1,C1),c(transport_ready,C),Error) :-
    !,    % One state at a time
    (  C1.proxy == none -> ByPassProxy = true;  ByPassProxy = false ),
    get_dict(host,C1,Host:Port),
    tcp_connect(Host:Port,Pair,[ bypass_proxy(ByPassProxy) ]),
    stream_pair(Pair,Read,_Write),
    tcp_getopt(Read,file_no(ReadFd)),
    put_dict(_{ read_stream_fd:   ReadFd,
                xmpp_server_live: true,
 	       raw_streams:      Pair }, C1, C),
    Error = none,
    debug(xmpp,"TCP raw streams ready",[]).



% % TLS negotiation
% % ===================
 xmpp_client(c(_S1,C1),c(transport_open, C1),Error) :- !,
    C1 = xmppclient{ jid: Jid,   password: Password,
       proxy: Proxy, host: Host:Port, tls: T,
       raw_streams: Pair, read_stream_fd: Fd, xmpp_server_live: L},
    Error = none,
    send_stream_request(Jid,Pair),
    debug(xmpp,"Plain raw streams ready",[]).

 xmpp_client(c(_S1,C1),c(transport_secured,C1),Error) :-
    !,    % One state at a time
    C1 = xmppclient{ jid: Jid,   password: Password,
                         proxy: Proxy, host: Host:Port, tls: tls(no_cert_verify),
 			raw_streams: Pair, read_stream_fd: Fd, xmpp_server_live: L
 		      },
    C = xmppclient{ jid: Jid,   password: Password,
                         proxy: Proxy, host: Host:Port, tls: tls(no_cert_verify),
 			raw_streams: TlsPair, read_stream_fd: Fd, xmpp_server_live: L
 		      },
    send_stream_request(Jid,Pair),
    % Response
    xml_in(C1,[//starttls/required->needed(starttls)],
          _Result,[context(stream_features)]),
    Error = none,
    tls_connect(Host,C1,TlsPair,[no_cert_verify,read_fd(Fd)]),  % TODO support pinned certs
    debug(xmpp,"TLS raw streams ready",[]).



% % SASL authentication
% % ===================

% xmpp_client(c(_S1,C1),c(authenticated,C1),Error) :-
%    !,    % One state at a time
%    % Plain mechanism offered
%    send_stream_request(C1.jid, C1.raw_streams),
%    xml_in(C1,[//mechanisms/mechanism(content=['PLAIN'])->needed(no_plain_sasl_offered)],
%           _,[context(stream_features)]),

%    % Send plain auth, accept success
%    jid(User,_Domain,_Resource) = C1.jid,
%    xml_out(sasl_auth(plain,User,C1.password), C1.raw_streams),
%    xml_in(C1,[//success->needed(no_authentication_success)]),
%    Error = none.


% % Bind resources
% % ===================
% xmpp_client(c(_S1,C1),c(resources_bound,C),Error) :-
%    !,    % One state at a time

%    % Receive new stream
%    send_stream_request(C1.jid, C1.raw_streams),
%    xml_in(C1,[  //'stream:stream'( @id=Id)->needed(not_authorized),
%                 //'stream:features'/bind  ->needed(no_bind_feature)
%              ],
%           _, [context(stream_features)]),
%    stanza_out(bind,C1,SId),
%    stanza_reply(iq(result,SId),C1,bind(Jid)),
%    jid_atom(Jid1,Jid),
%    put_dict(_{ stream_id: Id,
%                jid:       Jid1
% 	     }, C1, C),
%    Error=none.

% % Close
% % ===================
% xmpp_client(c(_S1,C1),c(not_connected,C1),Error) :-
%    Error=none,
%    close(C1.raw_streams). % The ssl layer will close the underlying TCP streams


                            /****************
                             *    Stanzas   *
                             ****************/

% Out
% ===========

%    XMPP Core
%    ---------

%      iq set
% stanza_out(iq,set(Content),C,Id) :-
%    gensym(iqset,Id),
%    format(atom(XML),
%       "<iq xmlns='jabber:client' type='set' id='~w'>~w</iq>", [Id,Content]),
%    xml_out_(C.raw_streams,XML).

% %      message
% stanza_out(message(Type,To),Content,c(_,C),Id) :-
%    gensym(msg,Id),
%    jid_atom(C.jid,From),
%    format(atom(XML),
%       "<message from='~w' id='~w' to='~w' type='~w' xml:lang='en'>
%           ~w
%       </message>",
%       [From,Id,To,Type,Content]),
%    xml_out_(C.raw_streams,XML).

% %      Resource bind
% stanza_out(bind,C,Id) :-
%    Resource = 'cGx4bXBwCg',
%    %Resource = 'gajim.HBJM4J6K',
%    format(string(Content),
%       "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>~w</resource></bind>",[Resource]),
%    stanza_out(iq,set(Content),C,Id).


% In
% -----------

%      bind result
stanza_reply(iq(result,Id),C,bind(Jid)) :-
   xml_in(C,[  //iq(@type=result,@id=Id)  ->needed(stanza_wrong_id),
               //bind/jid(content=[Jid])  ->needed(no_bind_reply_with_jid)
            ]).



%                               /************
%                                *    XML   *
%                                ************/

% xml_out(stream(From,To,_Id),Pair) :-
%    format(atom(XML),
%         "<?xml version='1.0'?><stream:stream from='~w' to='~w' version='1.0' xml:lang='de-DE' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>",
% 	[From,To]),
%    xml_out_(Pair,XML).

% xml_out(starttls,Pair) :-
%    format(atom(XML),"<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls' />",[]),
%    xml_out_(Pair,XML).

% xml_out(sasl_auth(plain,User,Pwd),Pair) :-
%    sasl_auth_xml(plain,User,Pwd,XML),
%    xml_out_(Pair,XML).



% xml_out_(Pair,XML) :-
%    write(Pair,XML),
%    flush_output(Pair),
%    debug(xmpp(xml),"Outgoing:~n~w~n",[XML]).

% xml_in(Client,Handlers) :-
%    xml_in(Client,Handlers,_Results,[]).

% xml_in(Client,Handlers,Results) :-
%    xml_in(Client,Handlers,Results,[]).

% xml_in(Client,Handlers,Results,Options) :-
%    Read = Client.raw_streams,
%    (  option(context(Context),Options)
%    -> true
%    ;  Context = in_stream
%    ),
%    xml_in_(Client,Context,Read,Handlers,Results,_DOM).


% xml_in_(Client,Context,Read,Handlers,Results,DOM) :-
%    xml_source(Client,Context,Read,Read1),
%    new_sgml_parser(Parser,[]),
%    set_sgml_parser(Parser,dialect(xml)),
%    set_sgml_parser(Parser,space(sgml)),
%    sgml_parse(Parser,
%               [ source(Read1),
% 	        parse(content),
% 		document(DOM),
% 		call(error,on_error)
% 	      ]),
%    nb_setval(xmpp_stream_in,xmpp{result:[]}),
%    call_handlers( DOM, Handlers),
%    nb_getval(xmpp_stream_in,XMPPStreamDict),
%    Results = XMPPStreamDict.result,
%    free_sgml_parser(Parser).


% on_error(Severity, Message, _Parser) :-
%    end_tag('stream:stream',Message),
%    !,
%    debug(xmpp(xml),'received error: ~w ~w~n',[Severity,Message]).

% on_error(Severity, Message, _Parser) :-
%    debug(xmpp(xml),'received error: ~w ~w~n',[Severity,Message]).

% end_tag(Tag,Msg) :-
%    format(string(Expected),'Ignored end-tag for "~w" which is not open',[Tag]),
%    sub_string(Msg,_,_,_,Expected).


% call_handlers( DOM, Handlers) :-
%    maplist(call_handler_( DOM),Handlers,Responses1),
%    exclude([E]>>(E=='$xpath-no-match$'),Responses1,Responses),
%    stream_in_set(result,[result=Responses]).

% call_handler_( DOM, Handler, Response) :-
%    debug(xmpp(handler),'Handler : ~w',[Handler]),
%    with_output_to(string(DOM1),
%       print_term(DOM,[output(current_output),left_margin(3)])),
%    indent_lines("   ",DOM1,DOM2),
%    debug(xmpp(handler),'Handler dom in : ~w',[DOM2]),
%    call_handler( DOM, Handler, Response),
%    debug(xmpp(handler),'Handler result: ~w',[Response]).

% call_handler( DOM, XPath->needed(Response), '$handler-match$') :-
%    !,  % only one 'needed' handler
%    (  xpath_chk(DOM,XPath,_Content)
%    -> true
%    ;  throw(xmpp_error(handler_no_match,Response))
%    ).

% call_handler( DOM, XPath->throw(Error), Response) :-
%    !,  % only one 'throw' handler
%    (  xpath_chk(DOM,XPath,_Content)
%    -> throw(Error)
%    ;  Response = na
%    ).

% call_handler( DOM, XPath->call(Goal), Response) :-
%    !,  % only one 'call' handler
%    (  xpath_chk(DOM,XPath,Content)
%    -> call(Goal,DOM,Content,Response)
%    ;  Response = na
%    ).


% stream_in_set(-Attr,Attrs) :-
%    !, ignore(stream_in_set(Attr,Attrs)).

% stream_in_set(Attr,Attrs) :-
%    memberchk(Attr=Val,Attrs),
%    nb_getval(xmpp_stream_in,S),
%    put_dict(Attr,S,Val,S1),
%    nb_setval(xmpp_stream_in,S1),
%    format('stream: ~w',[S1]).



                      /****************************
                       *    XMPP protocol utils   *
                       ****************************/

% send_stream_request(Jid,Pair) :-
%    %Request
%    Jid = jid(_User,Domain,_Res),
%    jid_atom(Jid,JidAtom),
%    xml_out(stream(JidAtom,Domain,_Id),Pair).


%TODO support non tls connection when client.tls == nones

% tls_connect(Host,Client,TlsPair,Opts) :-
%    (  option(no_cert_verify,Opts)
%    -> CertHook = cert_accept_any
%    ;  CertHook = cert_accept_any   % TODO support pinned certs
%    ),
%    xml_out(starttls,Client.raw_streams),
%    xml_in(Client,[//proceed->needed(no_starttls)]),
%    ssl_context(client, Ssl, [ host(Host), require_crl(false), close_parent(true),
%                               cert_verify_hook(CertHook)
%                               %,min_protocol_version(tlsv1_1)
% 			    ]),
%    stream_pair(Client.raw_streams,Read,Write),
%    ssl_negotiate(Ssl,Read,Write,TlsRead,TlsWrite),
%    !,
%    stream_pair(TlsPair,TlsRead,TlsWrite).


                             /**************
                              *    Utils   *
                              **************/


% We turn the available input into a string
% (exposed as a stream) for these reasons:
% 1. So that we can print incoming xml in if
%    debug(xmpp(xml)) is enabled
% 2. To support other transports besides connection-oriented
%    TCP, by only using string snippets
% xml_source(C,Type,StreamIn,StreamOut) :-
%    % Set timeout to infinite  if we are just waiting
%    % for stanzas or to a few seconds otherwise
%    (  Type == wait_for_stanza
%    -> set_stream(StreamIn,timeout(infinite))
%    ;  set_stream(StreamIn,timeout(10))
%    ),
%    % Search for stream:features end tag if we are
%    % negotiating a stream, otherwise search for
%    % a complete element
%    (  Type == stream_features
%    -> peek_tag(C,StreamIn,"\\/stream:features>|\\/stream:stream>",Length,XML)
%    ;  peek_tag(C,StreamIn,"^<(\\S+)[^>]*>.*</\\1\s*>|<[^<]+/\\s*>",Length,XML)  % closed xml element
%    ),
%    % This is to move the stream position to the proper place
%    % seek/4 didn't work at first try, so I just decided to do
%    % it in this less efficient way
%    read_string(StreamIn,Length,_),
%    debug(xmpp(xml),"Incoming:~n'~w'",[XML]),
%    % Produce a stream which reaches the end when the XML snippet ends
%    open_string(XML,StreamOut).


% % peek_tag(C,Stream,Regex,Len,XML) :-
% %    debugging(xmpp(xml),false),
% %    between(1,4096,Len),
% %    peek_string(Stream,Len,XML),
% %    re_match(Regex,XML),
% %    !.  % Once a match is found, don't try larger lengths

% % TODO: this is ugly see if it can be fixed
% peek_tag(C,Stream,Regex,Len,XML) :-
%    %debugging(xmpp(xml)),
%    nb_setval(xml0,XML),
%    between(1,4096,Len),
%    nb_getval(xml0,XML0),
%    catch(peek_string(Stream,Len,XML),E,
%       ( debug(xmpp(xml),"XML received on exception: '~w'",[XML0]),
%         (  XML0 == " "
% 	-> ( XML = XML0, xmpp_server_status(C,true) )
%         ;  throw(E)
% 	)
%       )
%    ),
%    (  XML \== " "
%    -> ( nb_setval(xml0,XML), re_match(Regex,XML) )
%    ;  true
%    ),
%    !.  % Once a match is found, don't try larger lengths


% xmpp_server_status(C,true) :-
%    b_set_dict(xmpp_server_live,C,true),
%    debug(xmpp,"XMPP Server is live",[]).

% xmpp_server_status(C,false) :-
%    b_set_dict(xmpp_server_live,C,false),
%    debug(xmpp,"XMPP Server is dead",[]).


% jid_atom(jid(none,Domain,none),Atom) :-
%    var(Atom), !, format(atom(Atom),'~w',[Domain]).

% jid_atom(jid(User,Domain,none),Atom) :-
%    var(Atom), !, format(atom(Atom),'~w@~w',[User,Domain]).

% jid_atom(jid(User,Domain,Resource),Atom) :-
%    var(Atom), !, format(atom(Atom),'~w@~w/~w',[User,Domain,Resource]).

% jid_atom(jid(User,Domain,Resource),Jid) :-
%    nonvar(Jid),
%    re_matchsub('(.*)@(.*)/(.*)',Jid,D,[]),
%    D = re_match{ 0:_, 1: User, 2: Domain, 3: Resource },
%    !.
% jid_atom(jid(User,Domain,_),Jid) :-
%    nonvar(Jid),
%    re_matchsub('(.*)@(.*)',Jid,D,[]),
%    D = re_match{ 0:_, 1: User, 2: Domain },
%    !.
% jid_atom(jid(none,Domain,none),Domain) :-
%    nonvar(Domain), !.



                      /****************************
                       *   Authentication utils   *
                       ****************************/

% sasl_prep(StrIn,StrIn). %  TODO: Implement SASLprep https://tools.ietf.org/html/rfc4013

% sasl_plain_userpwd(User,Pwd,Base64) :-
%    sasl_prep(User,User1),
%    sasl_prep(Pwd,Pwd1),
%    format(atom(PlainSASL),"\x00~w\x00~w",[User1,Pwd1]),
%    base64(PlainSASL,Base64).

% sasl_auth_xml(plain,User,Pwd,XML) :-
%    sasl_plain_userpwd(User,Pwd,Base64),
%    format(string(XML),
%       "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN'>~w</auth>",
%       [Base64]).



                             /**************
                              *   Config   *
                              **************/
% load_config(default,Options) :-
%    !,
%    config_file(File),
%    load_config(File,Options).

% load_config(File,Options) :-
%    option(encrypted_config(true),Options,true),
%    !,
%    gpg_load_file(File,[module(xmpp_client)]).

% load_config(File,_Options) :-
%    load_files(xmpp_client:File, [ sandboxed(true),
%                                   if(not_loaded)
%                                 ] ).

% config_file(File) :-
%    system:'$xdg_directory'(config,D),
%    atomic_list_concat([D,'/','plxmpp/config.pl'],File),
%    atomic_list_concat([D,'/','plxmpp'],Dir),
%    make_directory_path(Dir).


			    /****************
                             *   Messages   *
                             ****************/
:- multifile prolog:message//1.

prolog:message(xmpp_error(unable_to_handle_event(Event))) -->
   [ 'Unable to handle event: ~w'-[Event] ].
