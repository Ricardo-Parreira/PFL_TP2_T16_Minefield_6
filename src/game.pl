:- consult(utils).
:- consult(game_config).
:- use_module(library(between)).

switch_player('Player1', 'Player2').
switch_player('Player2', 'Player1').

display_game([Board, Player]):- 
    nl,  % Add an extra line for separation
    write('Current Player: '), 
    write(Player), 
    nl, 
    write('Game Board: '), 
    nl, 
    print_board(Board), 
    nl.

within_bounds(Board, Row, Col) :-
    length(Board, Size),
    between(1, Size, Row),
    between(1, Size, Col).

cell_empty(Board, Row, Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, empty).

set_cell(Board, Row, Col, Value, NewBoard) :-
    nth1(Row, Board, OldRow),
    nth1(Col, OldRow, _, RestRow),
    nth1(Col, NewRow, Value, RestRow),
    nth1(Row, Board, _, RestBoard),
    nth1(Row, NewBoard, NewRow, RestBoard).

is_valid_move(Board, Row, Col, Player) :-
    within_bounds(Board, Row, Col),
    cell_empty(Board, Row, Col),
    \+ creates_hard_corner(Board, Row, Col, Player),
    \+ creates_switch(Board, Row, Col, Player).

valid_moves([Board, CurrentPlayer | _], ValidMoves) :-
    findall([Row, Col], is_valid_move(Board, Row, Col, CurrentPlayer), ValidMoves).


move([Board, CurrentPlayer ], [Row, Col], [NewBoard, NextPlayer ]) :-
    valid_moves([Board, CurrentPlayer ], ValidMoves),
    member([Row, Col], ValidMoves),  
    set_cell(Board, Row, Col, CurrentPlayer, NewBoard), 
    switch_player(CurrentPlayer, NextPlayer).

game_cycle(GameState):-
    display_game(GameState) .    
play :-
                       
    draw_welcome,                    
    choose_menu(Menu),               % Let the player choose a menu.
    handle_menu(Menu),               % Handle the chosen menu option.
    Menu = 3,                        
    !.         
