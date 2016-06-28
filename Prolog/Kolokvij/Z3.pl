:- dynamic položen/1.
:- retractall(položen(_)).

prethodnik(ma,ts).
prethodnik(la,ts).
prethodnik(ts,ml).
prethodnik(ml,izr).
prethodnik(izr,sa).
prethodnik(ip,sa).

fali(Y,X) :- prethodnik(Y,X), \+ položen(Y), writef('Treba prvo položiti %w', [Y]).

položi(X) :- \+ fali(_,X), assertz(položen(X)).