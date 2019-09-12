unify(X, X).

member(X, list(X, _Rest)).
member(X, list(_Y, Rest)) :-
    member(X, Rest).

append(nil, Zs, Zs).
append(list(X, Xs), Ys, list(X, Zs)) :-
    append(Xs, Ys, Zs).
