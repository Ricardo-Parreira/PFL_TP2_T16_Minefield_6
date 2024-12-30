:- consult(utils).
:- consult(game_config).
:- use_module(library(between)).

switch_player('Black', 'White').
switch_player('White', 'Black').

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
    cell_empty(Board, Row, Col).
    %\+ creates_hard_corner(Board, Row, Col, Player),
    %\+ creates_switch(Board, Row, Col, Player).

valid_moves([Board, CurrentPlayer | _], ValidMoves) :-
    findall([Row, Col], is_valid_move(Board, Row, Col, CurrentPlayer), ValidMoves).

move([Board, CurrentPlayer], _, [Board, NextPlayer]) :-
    valid_moves([Board, CurrentPlayer], []),
    switch_player(CurrentPlayer, NextPlayer).

move([Board, CurrentPlayer ], [Row, Col], [NewBoard, NextPlayer ]) :-
    valid_moves([Board, CurrentPlayer ], ValidMoves),
    member([Row, Col], ValidMoves),  
    set_cell(Board, Row, Col, CurrentPlayer, NewBoard), 
    switch_player(CurrentPlayer, NextPlayer).

choose_move([Board, CurrentPlayer], 0, Move) :- 
    write('Enter your move as Row,Col: '), 
    read(Input),
    parse_input(Input, Move),
    (
        valid_moves([Board, CurrentPlayer], ValidMoves),
        member(Move, ValidMoves)
    ->  true  
    ;   write('Invalid move. Try again.'), nl, 
        choose_move([Board, CurrentPlayer], 0, Move)  
    ).

choose_move([Board, CurrentPlayer], 1, Move) :-
    valid_moves([Board, CurrentPlayer], ValidMoves),
    random_member(Move, ValidMoves).

game_cycle(GameState):-
    display_game(GameState) ,
    %game_over(GameState, Winner),
    valid_moves(GameState, ListOfMoves),
    choose_move(GameState, 0, Move),
    move(GameState, Move, NewGameState),
    game_cycle(NewGameState).


    
play :-
    draw_menu,                      
    choose_game_type(Type),         
    configure_game(Type, Config),    
    initial_state(Config, GameState),
    game_cycle(GameState),!.           
