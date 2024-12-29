game_type(N) :-
    print_text("Game type set to ", '', 0),
    write(N), nl.

choose_game_type(Type) :-
    repeat,
    read_input([Ascii | _]),
    to_number(Ascii, Type),
    (   between(1, 4, Type)
    ->  game_type(Type),!
    ;   write('Invalid choice, please try again.'), nl,
        fail 
    ).
configure_game(Type, Config) :-
    write('Enter board size (e.g., 5 for a 5x5 board): '), nl,
    read_input([Ascii | _]),
    to_number(Ascii, BoardSize),
    (   Type == 2 ; Type == 3 ; Type == 4
    ->  write('Enter difficulty level (1: Random, 2: Hard): '), nl,
        repeat,
        read_input([Ascii | _]),
        to_number(Ascii, Difficulty),
        (   between(1, 2, Difficulty)
        ->  !
        ;   write('Invalid choice, please try again.'), nl,
            fail 
        )
    ;   Difficulty = none 
    ),
    Config = [Type,BoardSize,Difficulty].

%GameState with Board, first player 
initial_state(Config, GameState) :-
    create_board(empty, BoardSize, Board),
    GameState = [Board,  'Player1'].

