strane(X=<Y,[X,Y]).
strane(X<Y,[X,Y]).
strane(X=:=Y,[X,Y]).
strane(X=\=Y,[X,Y]).
strane(X>Y,[X,Y]).
strane(X>=Y,[X,Y]).

svestrane([],[]).
svestrane([Prva|Ostale],[V1,V2|V3]) :- strane(Prva,[V1,V2]), svestrane(Ostale,V3).

razvrstaj([],[],[]).
razvrstaj([X|Ost],[X|Rez1],Rez2) :- var(X), razvrstaj(Ost,Rez1,Rez2).
razvrstaj([X|Ost],Rez1,[X|Rez2]) :- nonvar(X), razvrstaj(Ost,Rez1,Rez2).

varijable(L,R) :- svestrane(L,S), razvrstaj(S,V,B), sort(V,R).

maxbroj(L,M) :- svestrane(L,S), razvrstaj(S,V,B), max_list(B,M).

upto(N,[]).
upto(N,[Var|Rest]) :- between(1,N,Var), upto(N,Rest).

zadovolji([]).
zadovolji([First|Rest]) :- call(First), zadovolji(Rest).

riješi(L) :- varijable(L,Vars), length(Vars,N), maxbroj(L,M), NM is N+M,
   upto(NM,Vars), zadovolji(L), !.