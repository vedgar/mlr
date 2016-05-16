prirodni(0).

prirodni(slj(X)) :- prirodni(X).
% prirodni(X) -> prirodni(slj(X))

s(X, slj(X)).

zbr(X, 0, X).  % x + 0 = x
zbr(X, slj(Y), slj(Z)) :- zbr(X, Y, Z). % x+y=z -> x+y'=z'

mn(_, 0, 0).
mn(X, slj(Y), Z) :- zbr(T, X, Z), mn(X, Y, T).