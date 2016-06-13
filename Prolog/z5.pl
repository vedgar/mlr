:- set_prolog_flag(occurs_check, true).

manji(X, X + 1).
test :- manji(X + 1, X).