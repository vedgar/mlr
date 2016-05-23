:- use_module(library(clpfd)).

vreæe(V40, V60) :-
    V40 #>= 0, V60 #>= 0,
    V40 * 40 + V60 * 60 #= 500,
    label([V40, V60]).

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
        Vars = [S,E,N,D,M,O,R,Y],
        Vars ins 0..9,
        all_different(Vars),
                  S*1000 + E*100 + N*10 + D +
                  M*1000 + O*100 + R*10 + E #=
        M*10000 + O*1000 + N*100 + E*10 + Y,
	M #\= 0,
        label(Vars).
