cesta(petrinja,sisak).
cesta(zagreb,lekenik).
cesta(zagreb,velikaGorica).
cesta(sisak,lekenik).
cesta(varaždin,èakovec).

brid(X,Y) :- cesta(X,Y).
brid(X,Y) :- cesta(Y,X).

put(X,X,_).
put(X,Y,Avoid) :- brid(X,Z), \+ member(Z,Avoid), put(Z,Y,[Z|Avoid]).

povezani(X,Y) :- put(X,Y,[]).
