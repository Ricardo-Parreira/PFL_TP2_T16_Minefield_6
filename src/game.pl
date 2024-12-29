:- consult(utils).
:- consult(game_config).
:- use_module(library(between)).

display_game([Board, Player]):- 
    nl,  % Add an extra line for separation
    write('Current Player: '), 
    write(Player), 
    nl, 
    write('Game Board: '), 
    nl, 
    print_board(Board), 
    nl.

start_game(GameState) :-
    display_game(GameState),
    game_cycle(GameState).

game_cycle(GameState) :-
    write("Hello").
    
play :-
    draw_menu,                      
    choose_game_type(Type),         
    configure_game(Type, Config),    
    initial_state(Config, GameState),
    start_game(GameState).           
