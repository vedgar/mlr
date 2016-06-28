:- dynamic polo�en/1.
:- retractall(polo�en(_)).

prethodnik(ma,ts).
prethodnik(la,ts).
prethodnik(ts,ml).
prethodnik(ml,izr).
prethodnik(izr,sa).
prethodnik(ip,sa).

fali(Y,X) :- prethodnik(Y,X), \+ polo�en(Y), writef('Treba prvo polo�iti %w', [Y]).

polo�i(X) :- \+ fali(_,X), assertz(polo�en(X)).