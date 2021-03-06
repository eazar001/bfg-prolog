captain('Rocinante', 'James Holden').
captain('Canterbury', 'McDowell').
executive_officer('Rocinante', 'Naomi Nagata').
executive_officer('Canterbury', vacant).
pilot('Rocinante', 'Alex Kamal').
pilot('Canterbury', 'Alex Kamal').
mechanic('Canterbury', 'Amos Burton').
mechanic('Rocinante', 'Amos Burton').
whitelist('James Holden').
whitelist('Amos Burton').
whitelist('McDowell').
whitelist('Naomi Nagata').

leader(X) :-
    captain(S, X),
    whitelist(X).

leader(X) :-
    executive_officer(S, X),
    whitelist(X).
