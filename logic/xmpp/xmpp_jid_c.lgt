:- category(xmppJid).
	:- public([ jid_atom/2, jid/3]).

	:- use_module(pcre, [re_matchsub/4]).
	:- dynamic jid/3.


	jid_atom(jid(none, Domain, none), Atom) :-
		var(Atom), !, format(atom(Atom),'~w',[Domain]).
		
	jid_atom(jid(User, Domain, none), Atom) :-
		var(Atom), !, format(atom(Atom),'~w@~w',[User, Domain]).

	jid_atom(jid(User, Domain, Resource), Atom) :-
		var(Atom), !, format(atom(Atom),'~w@~w/~w',[User, Domain, Resource]).

	jid_atom(jid(User, Domain, Resource), Jid) :-
		nonvar(Jid),
		re_matchsub('(.*)@(.*)/(.*)', Jid, D, []),
		D = re_match{ 0:_, 1: User, 2: Domain, 3: Resource },
		!.
	jid_atom(jid(User, Domain, _), Jid) :-
		nonvar(Jid),
		re_matchsub('(.*)@(.*)', Jid, D, []),
		D = re_match{ 0:_, 1: User, 2: Domain },
		!.
	jid_atom(jid(none, Domain, none), Domain) :-
		nonvar(Domain), !.

:- end_category.
