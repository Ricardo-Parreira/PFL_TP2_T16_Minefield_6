:- consult(utils).
game_type(N) :-
    print_text("Game type set to ", '', 0),
    write(N), nl.

choose_game_type(Type) :-
    repeat,
    read_number(Type),
    (   between(1, 4, Type)
    ->  game_type(Type),!
    ;   write('Invalid choice, please try again.'), nl,
        fail 
    ).
valid_board_size(BoardSize):-
    BoardSize = 10; BoardSize = 13; BoardSize = 16.
    
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



%GameState with Board, first player 
initial_state([Type,BoardSize,Difficulty], GameState) :-
    create_board(empty, BoardSize, Board),
    GameState = [Board,  'Black'].
    

