rastav([],[],[]).
rastav([X|R],[X|A],B) :- rastav(R,A,B).
rastav([X|R],A,[X|B]) :- rastav(R,A,B).

praviRastav(L,A,B) :- rastav(L,A,B), A=[_|_], B=[_|_].

generiraj(L,X,Y) :- praviRastav(L,L1,L2), izraz(L1,X), izraz(L2,Y).

izraz([X],X).
izraz(L,X+Y) :- generiraj(L,X,Y).
izraz(L,X-Y) :- generiraj(L,X,Y).
izraz(L,X*Y) :- generiraj(L,X,Y).

% ako nemamo dijeljenje, ovdje mo�emo stati
% dobij(L,X) :- izraz(L,I), X is I, write(I).
% ina�e ne mo�emo koristiti is, nego moramo napisati svoj evaluator

izraz(L,X/Y) :- generiraj(L,X,Y).

izra�unaj(X+Y,Z) :- !, izra�unaj(X,X1), izra�unaj(Y,Y1), X1 =< Y1, Z is X1+Y1.
izra�unaj(X-Y,Z) :- !, izra�unaj(X,X1), izra�unaj(Y,Y1), Z is X1-Y1.
izra�unaj(X*Y,Z) :- !, izra�unaj(X,X1), izra�unaj(Y,Y1), X1 =< Y1, Z is X1*Y1.
izra�unaj(X/Y,Z) :- !, izra�unaj(X,X1), izra�unaj(Y,Y1), Y1 =\= 0, Z is X1 rdiv Y1.
izra�unaj(X,X).

dobij(L,X) :- izraz(L,I), izra�unaj(I,X), write(I).