:- consult(utils).
game_type(N) :-
    print_text("Game type set to ", '', 0),
    write(N), nl.

choose_game_type(Type) :-
    read_number(Type),
    valid_game_type(Type),
    game_type(Type).

choose_game_type(_) :-
    write('Invalid choice, please try again.'), nl,
    choose_game_type(_).

valid_game_type(Type) :-
    between(1, 4, Type).

valid_board_size(BoardSize):-
    BoardSize = 10. 
valid_board_size(BoardSize):-
    BoardSize = 13.
valid_board_size(BoardSize):-
    BoardSize = 16. 
configure_game(Type, Config) :-
    write('Enter board size (10, 13 or 16): '), nl,
    repeat,
    read_number(BoardSize),
    ( valid_board_size(BoardSize)
    -> !  
    ; write('Invalid board size, please try again.'), nl,
      fail
    ),
    (   (Type == 2 ; Type == 3 ; Type == 4)
    ->  write('Enter difficulty level (1: Random, 2: Hard): '), nl,
        repeat,
        read_number(Difficulty),
        (   between(1, 2, Difficulty)
        ->  !  
        ;   write('Invalid choice, please try again.'), nl,
            fail  
        )
    ;   Difficulty = none
    ),
    
    Config = [Type, BoardSize, Difficulty].



% Set up the initial game state
initial_state([Type, BoardSize, Difficulty], GameState) :-
    create_board(empty, BoardSize, Board),  
    players(Type, Difficulty, Players),  
    GameState = [Board,'Black',Players].

% Configure players based on the game type
players(1, _, [['Black', 0], ['White', 0]]) :- !. 
players(2, Difficulty, [['Black', 0], ['White', Difficulty]]) :- !. 
players(3, Difficulty, [['Black', Difficulty], ['White', 0]]) :- !. 
players(4, Difficulty, [['Black', Difficulty], ['White', Difficulty]]) :- !. 
    

