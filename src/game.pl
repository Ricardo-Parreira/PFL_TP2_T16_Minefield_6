
default(empty).

char(b, 'B').
char(w, 'W').
char(empty, '#').

player(1, player1).
player(2, player2).

board(2, [[empty, empty], [empty, empty]]).
board(3, [[empty, empty, empty], [empty, empty, empty], [empty, empty, empty]]).


create_list(_, 0, []).
create_list(Element, Size, [Element|Sublist]):-
    Size > 0,
    Size1 is Size - 1,
    create_list(Element, Size1, Sublist).

create_board(Element, Size, Board):-
    create_list(Element, Size, List),
    create_list(List, Size, Board).


%%% print the board

print_item(Item):- char(Item, C), write(C).

print_full_list([]):- write(''), !.
print_full_list([Item]):- !, print_item(Item).
print_full_list([Item|Rest]):- print_item(Item), write(' '), print_full_list(Rest).

print_board([]):- write(''), !.
print_board([Row]):- !, print_full_list(Row).
print_board([Row|Rest]):- print_full_list(Row), nl, print_board(Rest).

/* ############ DRAW THE MENU ############## */

% these are all from the practical sheets from class
print_n(0, _):- write(''), !.
print_n(N, S):- write(S), N1 is N-1, print_n(N1, S).

print_text(T, S, N) :- write(S), print_n(N, ' '), maplist(put_code, T), print_n(N, ' '), write(S).
print_banner(H, S, N) :- print_border(H,S,N), nl,print_filler(H, S, N),nl, print_text(H,S,N),nl, print_filler(H,S,N),nl, print_border(H,S,N).
print_border(H, S, N):- max_length(H, L) ,M is L + N + N + 2,print_n(M, S).
print_filler(H, S, N):-  max_length(H, L) ,M is L + N + N, write(S), print_n(M, ' '), write(S).

max_length([], 0).
max_length([Item], Length) :- length(Item, L), Length is L.
max_length([Item|Rest], Length) :- 
    length(Item, L), 
    max_length(Rest, RestMax),
    Length is max(RestMax, L) .

print_multi_text([], _, _, _).
print_multi_text([Text], Symbol, Padding, MaxLength) :-
    write(Symbol), 
    length(Text, L),
    Paux is MaxLength - L,
    P is Paux//2 + Padding,
    print_n(P, ' '),
    maplist(put_code, Text), 
    print_n(P, ' '), 
    write(Symbol).

print_multi_text([Text|Rest], Symbol, Padding, MaxLength) :-  
    write(Symbol), 
    length(Text, L),
    Paux is MaxLength - L,
    P is Paux//2 + Padding,
    print_n(P, ' '),
    maplist(put_code, Text), 
    print_n(P, ' '), 
    write(Symbol), nl, 
    print_multi_text(Rest, Symbol, Padding, MaxLength).

print_multi_banner(List, Symbol, Padding):-
    max_length(List, MaxLength), nl,
    print_border(List,Symbol,Padding), nl,
    print_filler(List, Symbol, Padding), nl, 
    print_multi_text(List,Symbol,Padding,MaxLength), nl, 
    print_filler(List,Symbol,Padding), nl, 
    print_border(List,Symbol,Padding), nl.


% têm de ter todos tamanho par ou impar depois resolvo isto
draw_menu :- print_multi_banner(["MINEFIELD",
                                " ",
                                "MENU ", 
                                "Welcome player!",
                                "Please choose which game type you would like to play:",
                                "1.Human vs Human ",
                                "2.Human vs PC",
                                "3.PC vs Human",
                                "4.PC vs PC "], '*', 4 ).


/* ####################################### */


/* ############## USER INPUT ############### */

% function from the class
read_number(N, N) :- peek_code(10), !.
read_number(N, Acc) :- get_code(X), X1 is X - 48, between(0, 9, X1), Acc1 is Acc *10 + X1, read_number(N, Acc1).

% turns the users input into a list filled with the ascii values 
read_input([]) :- peek_code(10), !.
read_input(AsciiList) :- peek_code(10), !.
read_input([X|Rest]) :- get_code(X), read_input(Rest).

%takes an Ascii value and passes it to the number
to_number(Ascii, Number) :- Number is Ascii - 48. 

/* ######################################*/


/* ################# GAME STATE ############## */

game_type(N) :- print_text("Game type set to ", '', 0), write(N).

play:-
    draw_menu,
    read_input([Ascii | N]), 
    to_number(Ascii, Type),
    game_type(Type).
