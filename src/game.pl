:- consult(utils).
:- use_module(library(between)).

game_type(N) :- print_text("Game type set to ", '', 0), write(N).

play :-
    draw_menu,
    repeat,
    read_input([Ascii | _]),
    to_number(Ascii, Type),
    (  between(1, 4, Type)
    ->  game_type(Type), 
        ! 
    ;   write('Invalid choice, please try again.'), nl,
        fail 
    ).