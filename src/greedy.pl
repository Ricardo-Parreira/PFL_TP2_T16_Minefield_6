:-consult(utils).

%get opponent
opponent(b, w).
opponent(w, b).

% base cases for when the game ends
value([Board, _, _], b, 10000) :-
    vertical_wins(Board, b), !.

value([Board, _, _], w, 10000) :-
    transpose(Board, Transposed),
    vertical_wins(Transposed, w), !.

value([Board, _, _], Player, -10000) :-
    opponent(Player, Opponent),
    value([Board, _, _], Opponent, 10000), !.

% value for intermediate states
value([Board, _, _], b, Value) :-
    progression_score(Board, b, ProgressionScore),
    %connection_score(Board, b, ConnectionScore),
    potential_score(Board, b, PotentialScore),
    Value is ProgressionScore + PotentialScore.

value([Board, _, _], w, Value) :-
    transpose(Board, Transposed),
    progression_score(Transposed, w, ProgressionScore),
    connection_score(Transposed, w, ConnectionScore),
    potential_score(Transposed, w, PotentialScore),
    Value is ProgressionScore + PotentialScore + ConnectionScore.

/*get the best move acoording to value */
%base case no more moves to explore
best_moves(_, [], _, Moves, Moves).

%the value of the new play is the same as the max score so we add the move to the list
best_moves([Board, CurrentPlayer, _], [Move|Rest], MaxValue, Temp, Moves) :-
    color(CurrentPlayer, Colour),
    move([Board, CurrentPlayer, _], Move, NewGameState),
    value(NewGameState, Colour, Value),
    Value =:= MaxValue,
    best_moves([Board, CurrentPlayer, _], Rest, MaxValue, [Move | Temp], Moves).

%the value of the new play is greater than the max score so it is now the best move
best_moves([Board, CurrentPlayer, _], [Move|Rest], MaxValue, Temp, Moves) :-
    color(CurrentPlayer, Colour),
    move([Board, CurrentPlayer, _], Move, NewGameState),
    value(NewGameState, Colour, Value),
    Value > MaxValue,
    best_moves([Board, CurrentPlayer, _], Rest, Value, [Move], Moves).

%the value of the new play is less than the max score so we skip this one
best_moves([Board, CurrentPlayer, _], [Move|Rest], MaxValue, Temp, Moves) :-
    color(CurrentPlayer, Colour),
    move([Board, CurrentPlayer, _], Move, NewGameState),
    value(NewGameState, Colour, Value),
    Value < MaxValue,
    best_moves([Board, CurrentPlayer, _], Rest, MaxValue, Temp, Moves).


/* calculate the progression score based on how the pieces occupy the goal edges*/
progression_score(Board, b, Score):-
    progression_score_calc(Board, b, Score).
progression_score(Board, w, Score) :-
    transpose(Board, Transposed),
    progression_score_calc(Transposed, w, Score).

%default vertical calculator, will transpose board for white
progression_score_calc(Board, Colour, Score):-
    length(Board, Size),
    nth1(1 ,Board, TopRowList),
    nth1(Size, Board, BottomRowList),
    include(=(Colour), TopRowList, TopList), % Filter elements matching Colour
    include(=(Colour), BottomRowList, BottomList), % Filter elements matching Colour
    length(TopList, TopScore),
    length(BottomList, BotScore),
    Score is TopScore + BotScore.


/* Calculate the score based on the potential a piece has to create connections */
potential_score(Board, Colour, TotalScore) :-
    length(Board, Size), 
    findall(Score, (
        nth1(Row, Board, RowList),      % For each row
        nth1(Col, RowList, Colour),          % For each correct stone in the row
        neighbor_coords(Row, Col, Size, Neighbors),
        get_surrounding_moves(Board, Colour, Neighbors, [], Surrounding),
        length(Surrounding, Score) %gets the number of free spaces surrounding the piece
    ), AllScores),
    list_sum(AllScores, TotalScore).

/* calculate the score based on the different paths it has, how big they are and if they move optimally in the right direction*/
connection_score(Board, Colour, TotalScore) :-
    length(Board, Size), 
    findall(Score, (
        nth1(Row, Board, RowList),      % For each row
        nth1(Col, RowList, Colour),          % For each correct stone in the row
        dfs_score(Board, Row-Col, Row-Col, Size, [], Colour, 0.0, ScoreRaw),
        abs(ScoreRaw, Score)
    ), AllScores),
    list_sum(AllScores, TotalScore).

dfs_score(_, _, Row-Col, _, Visited, _, CurrentScore, CurrentScore) :-
    member(Row-Col, Visited). % If already visited, return the current score.

% visit the piece
dfs_score(Board, OldRow-OldCol, Row-Col, Size, Visited, Colour, CurrentScore, FinalScore) :-
    \+ member(Row-Col, Visited),
    write('dfs_score'),nl,
    write(Row), write(' from row '), write(OldRow), nl, 
    write('With score: '), write(CurrentScore), nl,
    neighbor_coords(Row, Col, Size, Neighbors),            % Get orthogonal neighbors
    score_calculator(OldRow, Row, CurrentScore, NewScore),
    explore_neighbors_score(Board, Neighbors, Size, [Row-Col | Visited], Colour, Row-Col, NewScore, FinalScore).

% Base case: No more neighbors to explore.
explore_neighbors_score(_, [], _, _, _, _, AccumulatedScore, AccumulatedScore). 
explore_neighbors_score(Board, [NextRow-NextCol | Rest], Size, Visited, Colour, Row-Col, CurrentScore, FinalScore) :-
    \+ member(NextRow-NextCol, Visited),
    nth1(NextRow, Board, RowList),
    nth1(NextCol, RowList, Colour), % visit point only if it has a piece of correct colour
    dfs_score(Board, Row-Col, NextRow-NextCol, Size, Visited, Colour, CurrentScore, NeighborScore), % Continue DFS
    explore_neighbors_score(Board, Rest, Size, Visited, Colour, Row-Col, NeighborScore, FinalScore). % Process the rest of the neighbors of the piece at Row-Col

%if a neighbour doesnt have the right stone in it continue
explore_neighbors_score(Board, [NextRow-NextCol | Rest], Size, Visited, Colour, Row-Col, CurrentScore, FinalScore) :-
    nth1(NextRow, Board, RowList),
    \+ nth1(NextCol, RowList, Colour), % visit point only if it has a piece of correct colour
    explore_neighbors_score(Board, Rest, Size, Visited, Colour, Row-Col, CurrentScore, FinalScore). % Process the rest of the neighbors of the piece at Row-Col

%if a neighbor was already visited, skip it
explore_neighbors_score(Board, [NextRow-NextCol | Rest], Size, Visited, Colour, Row-Col, CurrentScore, FinalScore) :-
    member(NextRow-NextCol, Visited),
    explore_neighbors_score(Board, Rest, Size, Visited, Colour, Row-Col, CurrentScore, FinalScore). % Process the rest of the neighbors of the piece at Row-Col


% add 1 when moving downwards
score_calculator(OldRow, NextRow, OldScore, NewScore) :-
    write('score_calculator'), nl,
    write('calculating for '), write(OldRow-NextRow), nl,
    RowDiff is NextRow - OldRow,
    RowDiff =:= 1,
    NewScore is OldScore + 100,
    write('scores: '), write(OldScore-NewScore), nl.

% add 1 when moving upwards
score_calculator(OldRow, NextRow, OldScore, NewScore) :-
    RowDiff is NextRow - OldRow,
    RowDiff =:= -1,
    NewScore is OldScore - 100.

% multiply by 1.005 when it goes in a neutral direction
%if it hasnt moved towards the goal yet the score will remain 0
score_calculator(OldRow, NextRow, OldScore, NewScore) :-
    RowDiff is NextRow - OldRow,
    RowDiff =:= 0,
    NewScore is OldScore * 1.005.

/* auxiliary stuff */

%check if a stone is completely blocked and cannot create any connection because no cordinates near it are valid
is_blocked_completely(Board, Row-Col, Size, Colour) :-
    neighbor_coords(Row, Col, Size, Neighbors),
    get_surrounding_moves(Board, Colour, Neighbors, [], Surrounding),
    Surrounding = []. %it is blocked completely if it has no valid moves around it

% get valid moves from a list of neighboring coordinates
get_surrounding_moves(_, _, [], Surrounding, Surrounding). % no more neighbors
% add if valid
get_surrounding_moves(Board, Colour, [Row-Col|Rest], Sur, Surrounding) :-
    is_valid_move(Board, Row, Col, Colour),
    Sur1 = [Row-Col|Sur],
    get_surrounding_moves(Board, Colour, Rest, Sur1, Surrounding).
%skip if not valid
get_surrounding_moves(Board, Colour, [Row-Col|Rest], Sur, Surrounding) :-
    \+ is_valid_move(Board, Row, Col, Colour),
    get_surrounding_moves(Board, Colour, Rest, Sur, Surrounding).

% to check if a list is a subset of another list
list_subset([], _). % Base case: empty list is always a subset
list_subset([X|Rest], List2) :-
    select(X, List2, ReducedList2), % Remove one occurrence of X from List2
    list_subset(Rest, ReducedList2). % Recursively check the rest