cesta(petrinja,sisak).
cesta(zagreb,lekenik).
cesta(zagreb,velikaGorica).
cesta(sisak,lekenik).

duljina(X,X,0).
duljina(X,Y,1) :- cesta(X,Y).
duljina(X,Y,1) :- cesta(Y,X).
duljina(X,Y,N) :- N >= 2, N1 is N-1, duljina(X,Z,N1), duljina(Z,Y,1).

povezani(X,Y,N) :- between(0,N,M), duljina(X,Y,M).