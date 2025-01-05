:- use_module(library(lists)).

default(empty).

color('Black', b).
color('White', w).

char(b, 'B').
char(w, 'W').
char(empty, '#').


create_list(_, 0, []).
create_list(Element, Size, [Element|Sublist]):-
    Size > 0,
    Size1 is Size - 1,
    create_list(Element, Size1, Sublist).
create_board(Element, Size, Board):-
    create_list(Element, Size, List),
    create_list(List, Size, Board).

print_item(Item) :-
    char(Item,New),
    write(New).

abs(Raw, Raw):-
    Raw >= 0.
abs(Raw, Abs):-
    Raw < 0,
    Abs is 0 - Raw.

% from class
list_sum([], 0).
list_sum([H | T], Sum) :- list_sum(T, S1), Sum is S1 + H.

print_full_list([]) :-
    nl.  

print_full_list([Item|Rest]) :-
    print_item(Item),
    write(' '),  
    print_full_list(Rest).


print_board([]).  

print_board([Row|Rest]) :-
    print_full_list(Row),  
    print_board(Rest).

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


read_number(N) :-
    read_number(N, 0). 

read_number(N, N) :- peek_code(10),get_code(_),  !.

read_number(N, Acc) :-
    get_code(X), 
    X >= 48, X =< 57, 
    X1 is X - 48,  
    Acc1 is Acc * 10 + X1,  
    read_number(N, Acc1).  

% turns the users input into a list filled with the ascii values 
% Lê os caracteres até a quebra de linha e retorna uma lista de códigos ASCII
read_input([]) :- 
    peek_code(10), 
    get_code(_),   
    !.
read_input([X|Rest]) :- 
    get_code(X), 
    read_input(Rest).

% Reading moves
parse_input((Row, Col), [Row, Col]) :- 
    integer(Row), integer(Col), !.  

parse_input(Row-Col, [Row, Col]) :- 
    integer(Row), integer(Col), !.  

parse_input([Row, Col], [Row, Col]) :- 
    integer(Row), integer(Col), !.  

parse_input(_, _) :- 
    write('Invalid input format. Use (Row,Col) , Row-Col or [Row, Col].'), nl, 
    fail.  
    
translate_input([Row, Col], [TranslatedRow, Col], Board) :-
    % Get the number of rows in the board.
    length(Board, NumRows),

    % The row index is reversed (bottom to top), so the translated row is NumRows - Row
    TranslatedRow is NumRows + 1 - Row.


%takes an Ascii value and passes it to the number
to_number(Ascii, Number) :- Number is Ascii - 48. 

/* ######################################*/


/* ##################### LOGIC TO DETECT WINNER ###################### */

neighbor(Row, Col, RowUp, Col) :- RowUp is Row - 1.
neighbor(Row, Col, RowDown, Col) :- RowDown is Row + 1.
neighbor(Row, Col, Row, ColLeft) :- ColLeft is Col - 1.
neighbor(Row, Col, Row, ColRight) :- ColRight is Col + 1.

neighbor_coords(Row, Col, Size, Neighbors) :-
    findall(
        R-C,
        (
            neighbor(Row, Col, R, C),       % Get a neighbor
            R > 0, R =< Size,               % Row is within bounds
            C > 0, C =< Size                % Column is within bounds
        ),
        Neighbors
    ).


vertical_wins(Board, Colour) :-
    length(Board, Size),
    nth1(1, Board, FirstRow),
    findall(StartCol, nth1(StartCol, FirstRow, Colour), Cols), % Find all correct stones in the first row
    member(StartCol, Cols),
    dfs_vertical(Board, 1, StartCol, Size, [], Colour).        % Start DFS from each stone

dfs_vertical(Board, Row, Col, Size, _, Colour) :-
    Row =:= Size,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Colour). % Ensure we have a stone in the last row

dfs_vertical(Board, Row, Col, Size, Visited, Colour) :-
    neighbor_coords(Row, Col, Size, Neighbors),
    explore_neighbors(Board, Neighbors, Size, [Row-Col | Visited], Colour).

explore_neighbors(_, [], _, _, _) :- fail.             
explore_neighbors(Board, [Row-Col | Rest], Size, Visited, Colour) :-
    \+ member(Row-Col, Visited),                   
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Colour),                         % Ensure the neighbor is a correct stone
    dfs_vertical(Board, Row, Col, Size, Visited, Colour);     % Continue DFS from this neighbor
    explore_neighbors(Board, Rest, Size, Visited, Colour). % Explore the remaining neighbors









