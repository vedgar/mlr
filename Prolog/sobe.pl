door(a, b).
door(b, e).
door(b, c).
door(d, e).
door(c, d).
door(e, f).
door(g, e).
hasPhone(g).

through(X, Y) :- door(X, Y).
through(X, Y) :- door(Y, X).

room(X) :- through(X, _).

% go(From, To, Avoid, Path) konstruira Path od From do To
% izbjegavajuæi sobe u listi Avoid (radi izbjegavanja
% beskonaène rekurzije).
go(X, X, _, []).
go(From, To, Avoid, [Aux|Rest]) :-
	through(From, Aux),
	\+ member(Aux, Avoid),
	go(Aux, To, [Aux|Avoid], Rest).
	