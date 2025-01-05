:- consult(utils).
:- consult(board).
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

configure_game(Type, Config) :-
    get_board_size(BoardSize),
    get_mode(Mode),
    get_difficulty(Type, Difficulty),
    Config = [Type, BoardSize, Difficulty,Mode].

get_board_size(BoardSize) :-
    write('Enter board size (10, 13 or 16): '), nl,
    repeat,
    read_number(BoardSize),
    valid_board_size(BoardSize), !. 

get_mode(Mode) :-
    write('Enter a mode (1: Beginner, 2: Normal): '), nl,
    repeat,
    read_number(Mode),
    valid_mode(Mode), !. 

valid_mode(Mode) :-
    member(Mode, [1,2]).

valid_mode(_) :-
    write('Invalid mode, please try again.'), nl,
    fail. 

valid_board_size(BoardSize) :-
    member(BoardSize, [10, 13, 16]).
valid_board_size(_) :-
    write('Invalid board size, please try again.'), nl,
    fail. 

get_difficulty(Type, Difficulty) :-
    requires_difficulty(Type),
    ask_difficulty(Difficulty).

get_difficulty(Type, none) :-
    \+ requires_difficulty(Type).

ask_difficulty(Difficulty) :-
    write('Enter a bot difficulty level (1: Random, 2: Greedy): '), nl,
    repeat,
    read_number(Difficulty),
    valid_difficulty(Difficulty), !. 

valid_difficulty(Difficulty) :-
    member(Difficulty, [1, 2]).
valid_difficulty(_) :-
    write('Invalid choice, please try again.'), nl,
    fail. % Force backtracking to repeat.

requires_difficulty(Type) :-
    member(Type, [2, 3, 4]).


% Set up the initial game state
initial_state([Type, BoardSize, Difficulty, Mode], GameState) :-
    %create_board(empty, BoardSize, Board),   % Create an initial board with the given size and empty configuration
    players(Type, Difficulty, Players),     % Set up the players based on the game type and difficulty level(for bots)
    board_black_win(Board),         % For demo
    GameState = [Board, 'Black', Players, Mode].  % Initialize the game state with the board, starting player ('Black'), and players list

% Configure players based on the game type
players(1, _, [['Black', 0], ['White', 0]]) :- !. 
players(2, Difficulty, [['Black', 0], ['White', Difficulty]]) :- !. 
players(3, Difficulty, [['Black', Difficulty], ['White', 0]]) :- !. 
players(4, Difficulty, [['Black', Difficulty], ['White', Difficulty]]) :- !. 
    

