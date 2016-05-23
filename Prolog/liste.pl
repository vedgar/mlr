duljina([], 0).
duljina([_|Ostatak], X) :- duljina(Ostatak, Y), plus(1, Y, X).

zbroj([], 0).
zbroj([Poèetak|Ostatak], S) :- zbroj(Ostatak, T), plus(T, Poèetak, S).

:- use_module(library(clpfd)).
zbr([], 0).
zbr([Poèetak|Ostatak], S) :- zbr(Ostatak, T), S #= T + Poèetak.

najm([X], X).
najm([X|Ost], X) :- najm(Ost, Y), X #< Y.
najm([X|Ost], Y) :- najm(Ost, Y), X #>= Y.

element(X, [Y|_]) :- unify_with_occurs_check(X, Y).
element(X, [_|Ost]) :- element(X, Ost).

konk([], L2, L2).
konk([X|Ost], L2, [X|NoviOst]) :- konk(Ost, L2, NoviOst).