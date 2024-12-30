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
    
set_cell([Row|Rest], 1, Col, Value, [NewRow|Rest]) :-  
    color(Value, Color),  
    set_cell_in_row(Row, Col, Color, NewRow).  

set_cell([Row|Rest], RowIndex, Col, Value, [Row|NewRest]) :- 
    RowIndex > 1,  
    NewRowIndex is RowIndex - 1, 
    set_cell(Rest, NewRowIndex, Col, Value, NewRest). 

set_cell_in_row([_Empty|Rest], 1, Color, [Color|Rest]).  
set_cell_in_row([Current|Rest], ColIndex, Color, [Current|NewRest]) :-
    ColIndex > 1,  
    NewColIndex is ColIndex - 1,  
    set_cell_in_row(Rest, NewColIndex, Color, NewRest).  


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

move([Board, CurrentPlayer], _, [Board, NextPlayer]) :-
    valid_moves([Board, CurrentPlayer], []),  % If no valid moves, just switch player
    switch_player(CurrentPlayer, NextPlayer).

move([Board, CurrentPlayer], [Row, Col], [NewBoard, NextPlayer]) :-
    write('Board before move: '), nl, print_board(Board), nl,
    valid_moves([Board, CurrentPlayer], ValidMoves),
    write('Valid moves: '), write(ValidMoves), nl,
    
    member([Row, Col], ValidMoves),  % Check if the move is valid
    
    
    set_cell(Board, Row, Col, CurrentPlayer, NewBoard),
    
    % Switch to the next player
    switch_player(CurrentPlayer, NextPlayer),
    
    % Print the current player and board state
    write('Next Player: '), write(NextPlayer), nl,
    write('Current Player: '), write(CurrentPlayer), nl,
    write('Move: '), write(Row-Col), nl,
    
    % Display the updated board
    write('Board after move: '), nl,
    print_board(NewBoard).  % This will print the updated board after the move




choose_move([Board, CurrentPlayer], 0, Move) :- 
    write('Enter your move as Row,Col: '), 
    read(Input),
    parse_input(Input, Move),
    valid_moves([Board, CurrentPlayer], ValidMoves),
    member(Move, ValidMoves),  
    !. 

choose_move([Board, CurrentPlayer], 0, Move) :- 
    choose_move([Board, CurrentPlayer], 0, Move).  



choose_move([Board, CurrentPlayer], 1, Move) :-
    valid_moves([Board, CurrentPlayer], ValidMoves),
    random_member(Move, ValidMoves).

game_cycle(GameState) :-
    display_game(GameState),  
    choose_move(GameState, 0, Move),  
    move(GameState, Move, NewGameState),!, 
    game_cycle(NewGameState).  


   
play :-
    draw_menu,                      
    choose_game_type(Type),!,         
    configure_game(Type, Config),!,    
    initial_state(Config, GameState),!,
    game_cycle(GameState).           
