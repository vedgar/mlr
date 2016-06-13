:- dynamic cache/1.

fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, X) :- cache(fibonacci(N, X)), !.
fibonacci(N, X) :- N > 1,
	N1 is N - 1,
	N2 is N - 2,
	fibonacci(N1, F1),
	fibonacci(N2, F2),
	X is F1 + F2,
	asserta(cache(fibonacci(N, X))).

catalan(0, 1).
catalan(N, X) :- cache(catalan(N, X)), !.
catalan(N, X) :- N > 0,
	N1 is N - 1,
	listaUmn(N1, N, L),
	zbroj(L, X),
	asserta(cache(catalan(N, X))).

zbroj([], 0).
zbroj([P|Ost], X) :-
	zbroj(Ost, Y),
	X is P + Y.

listaUmn(N, 0, []).
listaUmn(N, K, L) :-
	K > 0,
	K1 is K - 1,
	K2 is N - K1,
	catalan(K1, C1),
	catalan(K2, C2),
	Umn is C1 * C2,
	listaUmn(N, K1, L1),
	L = [Umn|L1].