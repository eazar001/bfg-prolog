captain('Rocinante', 'James Holden').
captain('Canterbury', 'McDowell').
executive_officer('Rocinante', 'Naomi Nagata').
executive_officer('Canterbury', vacant).
pilot('Rocinante', 'Alex Kamal').
pilot('Canterbury', 'Alexa Kamal').
mechanic('Canterbury', 'Amos Burton').
mechanic('Rocinante', 'Amos Burton').
whitelist('James Holden').
whitelist('Amos Burton').
whitelist('McDowell').
whitelist('Naomi Nagata').

unify(X, X).

member(X, list(X, _Rest)).
member(X, list(_Y, Rest)) :-
    member(X, Rest).

append(nil, Zs, Zs).
append(list(X, Xs), Ys, list(X, Zs)) :-
    append(Xs, Ys, Zs).

leader(X) :-
    captain(S, X),
    whitelist(X).

leader(X) :-
    executive_officer(S, X),
    whitelist(X).
