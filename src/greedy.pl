:-consult(utils).

value([Board, CurrentPlayer, _],b, 1) :-
    vertical_wins(Board, b), !.

value([Board, CurrentPlayer, _],b, -1) :-
    transpose(Board, Transposed),
    vertical_wins(Transposed, b), !.

value([Board, CurrentPlayer, _],w, 1) :-
    transpose(Board, Transposed),
    vertical_wins(Transposed, b), !.

value([Board, CurrentPlayer, _],w, -1) :-
    vertical_wins(Board, b), !.

value(_, _, 0).