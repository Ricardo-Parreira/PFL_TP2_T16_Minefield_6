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
    set_cell_in_row(Row, Col, Value, NewRow).  

set_cell([Row|Rest], RowIndex, Col, Value, [Row|NewRest]) :- 
    RowIndex > 1,  
    NextRowIndex is RowIndex - 1, 
    set_cell(Rest, NextRowIndex, Col, Value, NewRest).

set_cell_in_row([_|Rest], 1, Value, [Value|Rest]).  

% Avança para a próxima célula na linha
set_cell_in_row([Current|Rest], ColIndex, Value, [Current|NewRest]) :-
    ColIndex > 1,  
    NextColIndex is ColIndex - 1,  
    set_cell_in_row(Rest, NextColIndex, Value, NewRest). 


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
    valid_moves([Board, CurrentPlayer], ValidMoves),    
    member([Row, Col], ValidMoves),  
    color(CurrentPlayer,Value),
    set_cell(Board, Row, Col, Value, NewBoard),
    switch_player(CurrentPlayer, NextPlayer).



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
