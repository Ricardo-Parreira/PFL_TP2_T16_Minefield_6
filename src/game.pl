:- consult(utils).
:- consult(game_config).
:- use_module(library(between)).
:- use_module(library(random)).

switch_player('Black', 'White').
switch_player('White', 'Black').

get_difficulty_level([_, CurrentPlayer, Players], Difficulty) :-
    member([CurrentPlayer, Difficulty], Players).

display_game([Board, Player,_]):- 
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

valid_moves([Board, CurrentPlayer,_ | _], ValidMoves) :-
    findall([Row, Col], is_valid_move(Board, Row, Col, CurrentPlayer), ValidMoves).

move([Board, CurrentPlayer, Players], _, [Board, NextPlayer, Players]) :-
    valid_moves([Board, CurrentPlayer, Players], []), 
    write('You have no valid moves.'), nl,
    switch_player(CurrentPlayer, NextPlayer).

move([Board, CurrentPlayer, Players], [Row, Col], [NewBoard, NextPlayer, Players]) :-
    valid_moves([Board, CurrentPlayer, Players], ValidMoves),    
    member([Row, Col], ValidMoves),  
    color(CurrentPlayer, Value),
    set_cell(Board, Row, Col, Value, NewBoard),
    switch_player(CurrentPlayer, NextPlayer).


check_game_over([Board, CurrentPlayer,_]) :-
    valid_moves([Board, 'White'], []),
    valid_moves([Board, 'Black'], []),
    vertical_wins(Board, b),
    write('GAME OVER'), nl,
    write('Black won!').

check_game_over([Board, CurrentPlayer,_]) :-
    valid_moves([Board, 'White'], []),
    valid_moves([Board, 'Black'], []),
    vertical_wins(Board, w),
    write('GAME OVER'), nl,
    write('White won!').

check_game_over([Board, CurrentPlayer,_]) :-
    valid_moves([Board, 'White'], []),
    valid_moves([Board, 'Black'], []),
    write('GAME OVER'), 
    write('It is a draw!').

check_game_over([Board, CurrentPlayer,_]) :-
    vertical_wins(Board, b),
    write('GAME OVER'), nl,
    write('Black won!').

check_game_over([Board, CurrentPlayer,_]) :-
    transpose(Board, Transposed),
    vertical_wins(Transposed, w),
    write('GAME OVER'), nl,
    write('White won!').


choose_move([Board, CurrentPlayer,_], 0, Move) :- 
    repeat,
    write('Enter your move as Row,Col: '), 
    read(Input),
    parse_input(Input, Move), 
    valid_moves([Board, CurrentPlayer,_], ValidMoves),  
    ( member(Move, ValidMoves) ->  
        !,
        true  
    ; 
        write('Invalid move. Please try again.'), nl,
        fail 
    ).



choose_move([Board, CurrentPlayer,_], 1, Move) :-
    valid_moves([Board, CurrentPlayer,_], ValidMoves),
    random_member(Move, ValidMoves).

game_cycle(GameState) :-
    display_game(GameState), 
    check_game_over(GameState).

%game not over may proceed
game_cycle(GameState) :- 
    get_difficulty_level(GameState, Difficulty),
    choose_move(GameState, Difficulty, Move),  
    move(GameState, Move, NewGameState),!, 
    game_cycle(NewGameState).  

   
play :-
    draw_menu,                      
    choose_game_type(Type),!,         
    configure_game(Type, Config),!,    
    initial_state(Config, GameState),!,
    game_cycle(GameState).           
