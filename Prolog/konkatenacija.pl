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

slučajnaLista(0, []).
slučajnaLista(N, [X|Ostali]) :-
	plus(M, 1, N),
	X is random(100),
	slučajnaLista(M, Ostali).