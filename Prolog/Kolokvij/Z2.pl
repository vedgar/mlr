:- use_module(library(clpfd)).

sortirana(X) :- uzlazna(X).
sortirana(X) :- silazna(X).

uzlazna([]).
uzlazna([_]).
uzlazna([First|Rest]) :- Rest = [Second|_], First #=< Second, uzlazna(Rest).

silazna([]).
silazna([_]).
silazna([First|Rest]) :- Rest = [Second|_], First #>= Second, silazna(Rest).