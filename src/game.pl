:- consult(utils).
:- consult(game_config).
:- consult(greedy).
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

get_cell(Board, Row, Col, Value) :-
    nth1(Row, Board, RowList),  % Get the RowList at the specified Row
    nth1(Col, RowList, Value).  % Get the Value at the specified Column in the RowList

creates_hard_corner(Board, Row, Col, Player) :-
    color(Player, PlayerColor),                 
    switch_player(Player, Opponent),          
    color(Opponent, OpponentColor),           
    between(-1, 0, DR),                       
    between(-1, 0, DC),
    StartRow is Row + DR,                      
    StartCol is Col + DC,
    StartCol1 is StartCol+1,
    StartRow1 is StartRow+1,
    within_bounds(Board, StartRow, StartCol), 
    within_bounds(Board, StartRow1, StartCol1),
    get_cell(Board, StartRow, StartCol, V1),        
    get_cell(Board, StartRow, StartCol1, V2),
    get_cell(Board, StartRow1, StartCol, V3),
    get_cell(Board, StartRow1, StartCol1, V4),
    simulate_move_in_pattern([V1, V2, V3, V4], PlayerColor, OpponentColor, Row, Col, StartRow, StartCol).
    
simulate_move_in_pattern([V1, V2, V3, V4], PlayerColor, OpponentColor, Row, Col, StartRow, StartCol) :-
    % Simulate the effect of placing the player color
    (Row =:= StartRow, Col =:= StartCol -> NV1 = PlayerColor ; NV1 = V1),
    (Row =:= StartRow, Col =:= StartCol + 1 -> NV2 = PlayerColor ; NV2 = V2),
    (Row =:= StartRow + 1, Col =:= StartCol -> NV3 = PlayerColor ; NV3 = V3),
    (Row =:= StartRow + 1, Col =:= StartCol + 1 -> NV4 = PlayerColor ; NV4 = V4),
    % Check for hard corner pattern after simulated move
    hard_corner_pattern([NV1, NV2, NV3, NV4], PlayerColor, OpponentColor).

hard_corner_pattern([Player, Opponent, empty, Player], Player, Opponent).
hard_corner_pattern([Opponent, Player, empty, Opponent], Player, Opponent).

hard_corner_pattern([Player, empty, Opponent, Player], Player, Opponent).
hard_corner_pattern([Opponent, empty, Player, Opponent], Player, Opponent).

hard_corner_pattern([empty, Player, Player, Opponent], Player, Opponent).
hard_corner_pattern([empty, Opponent, Opponent, Player], Player, Opponent).

hard_corner_pattern([Opponent, Player, Player, empty], Player, Opponent).
hard_corner_pattern([Player, Opponent, Opponent, empty], Player, Opponent).

creates_switch(Board, Row, Col, Player) :-
    color(Player, PlayerColor),
    switch_player(Player, Opponent),
    color(Opponent, OpponentColor),
    % Check for 2x3 areas
    between(-1, 0, DR),
    between(-2, 0, DC), 
    StartRow is Row + DR,
    StartCol is Col + DC,
    StartRow1 is StartRow + 1,
    StartCol2 is StartCol + 2,
    within_bounds(Board, StartRow, StartCol),
    within_bounds(Board, StartRow1, StartCol2), % Bounds for 2x3 area
    get_switch_area_2x3(Board, StartRow, StartCol, SwitchArea2x3),
    switch_pattern(SwitchArea2x3, PlayerColor, OpponentColor).

creates_switch(Board, Row, Col, Player) :-
    color(Player, PlayerColor),
    switch_player(Player, Opponent),
    color(Opponent, OpponentColor),
    % Check for 2x4 areas
    between(-1, 0, DR),
    between(-3, 0, DC), 
    StartRow is Row + DR,
    StartCol is Col + DC,
    StartRow1 is StartRow + 1,
    StartCol3 is StartCol + 3,
    within_bounds(Board, StartRow, StartCol),
    within_bounds(Board, StartRow1, StartCol3), % Bounds for 2x4 area
    get_switch_area_2x4(Board, StartRow, StartCol, SwitchArea2x4),
    switch_pattern(SwitchArea2x4, PlayerColor, OpponentColor).

get_switch_area_2x3(Board, StartRow, StartCol, [C1, C2, C3, C4, C5, C6]) :-
    StartCol1 is StartCol + 1,
    StartRow1 is StartRow + 1,
    within_bounds(Board, StartRow, StartCol),
    within_bounds(Board, StartRow, StartCol1),
    within_bounds(Board, StartRow1, StartCol),
    within_bounds(Board, StartRow1, StartCol1),
    get_cell(Board, StartRow, StartCol, C1),        % Top-left
    get_cell(Board, StartRow, StartCol1, C2),      
    get_cell(Board, StartRow1, StartCol, C4),       % Bottom-left
    get_cell(Board, StartRow1, StartCol1, C5),      
    (StartCol2 is StartCol + 2, within_bounds(Board, StartRow, StartCol2) -> get_cell(Board, StartRow, StartCol2, C3) ; C2 = empty), % Top-right
    (StartCol2 is StartCol + 2, within_bounds(Board, StartRow1, StartCol2) -> get_cell(Board, StartRow1, StartCol2, C6) ; C5 = empty). % Bottom-right


get_switch_area_2x4(Board, StartRow, StartCol, [C1, C2, C3, C4, C5, C6, C7, C8]) :-
    StartCol1 is StartCol + 1,
    StartCol2 is StartCol + 2,
    StartCol3 is StartCol + 3,
    StartRow1 is StartRow + 1,
    within_bounds(Board, StartRow, StartCol),
    within_bounds(Board, StartRow1, StartCol),
    get_cell(Board, StartRow, StartCol, C1),        % Top-left corner
    (within_bounds(Board, StartRow, StartCol1) -> get_cell(Board, StartRow, StartCol1, C2) ; C2 = empty),  % Top second
    (within_bounds(Board, StartRow, StartCol2) -> get_cell(Board, StartRow, StartCol2, C3) ; C3 = empty),  % Top third
    (within_bounds(Board, StartRow, StartCol3) -> get_cell(Board, StartRow, StartCol3, C4) ; C4 = empty),  % Top-right corner
    get_cell(Board, StartRow1, StartCol, C5),       % Bottom-left corner
    (within_bounds(Board, StartRow1, StartCol1) -> get_cell(Board, StartRow1, StartCol1, C6) ; C6 = empty),  % Bottom second
    (within_bounds(Board, StartRow1, StartCol2) -> get_cell(Board, StartRow1, StartCol2, C7) ; C7 = empty),  % Bottom third
    (within_bounds(Board, StartRow1, StartCol3) -> get_cell(Board, StartRow1, StartCol3, C8) ; C8 = empty).  % Bottom-right corner


switch_pattern([Player, empty, Opponent, Opponent, empty,  Player], Player, Opponent).
switch_pattern([Opponent, empty, Player, Player, empty,  Opponent], Player, Opponent).

switch_pattern([Player, empty, empty, Opponent, Opponent, empty, empty,  Player], Player, Opponent).
switch_pattern([Opponent, empty, empty, Player, Player, empty, empty, Opponent], Player, Opponent).


is_valid_move(Board, Row, Col, Player) :-
    within_bounds(Board, Row, Col),
    cell_empty(Board, Row, Col),
    \+ creates_hard_corner(Board, Row, Col, Player),
    \+ creates_switch(Board, Row, Col, Player).

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
