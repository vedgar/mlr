:- use_module(library(clpfd)).

min2(X, Y, X) :- X #=< Y.
min2(X, Y, Y) :- X #> Y.

prodMin([X], X, X).
prodMin([Poc|Ost], Prod, Min) :-
	prodMin(Ost, Prod1, Min1),
	Prod #= Prod1 * Poc,
	min2(Min1, Poc, Min).