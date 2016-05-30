append([], B, B).
append([X|A], B, [X|Pom]) :-
	append(A, B, Pom).

% reverse([], []).
% reverse([X|A], Rez) :-
%	append(Pom, [X], Rez),
%	reverse(A, Pom).

reverse(L, Rez) :- reverse(L, [], Rez).
reverse([], Ak, Ak).
reverse([X|A], Ak, Rez) :- reverse(A, [X|Ak], Rez).

sluèajnaLista(0, []).
sluèajnaLista(N, [X|Ostali]) :-
	plus(M, 1, N),
	X is random(100),
	sluèajnaLista(M, Ostali).