:-consult(utils).

%get opponent
opponent(b, w).
opponent(w, b).

% base cases for when the game ends
value([Board, _, _,_], b, 10000) :-
    vertical_wins(Board, b), !.

value([Board, _, _,_], w, 10000) :-
    transpose(Board, Transposed),
    vertical_wins(Transposed, w), !.

value(GameState, Player, -10000) :-
    opponent(Player, Opponent),
    value(GameState, Opponent, 10000), !.

% value for intermediate states
value([Board, _, _,Mode], b, Value) :-
    progression_score([Board, _, _,_], b, ProgressionScore),
    connection_score([Board, _, _,_], b, ConnectionScore),
    potential_score([Board, _, _, Mode], b, PotentialScore),
    Value is ProgressionScore + ConnectionScore + PotentialScore. 
value([Board, _, _,Mode], w, Value) :-
    transpose(Board, Transposed),
    progression_score([Transposed, _, _,_], w, ProgressionScore),
    connection_score([Transposed, _, _,_], w, ConnectionScore),
    potential_score([Transposed, _, _,Mode], w, PotentialScore),
    Value is ProgressionScore + ConnectionScore + PotentialScore.

/*get the best move acoording to value */
%base case no more moves to explore
best_moves(_, [], _, Moves, Moves).

%extreme case to block the enemies win
best_moves([Board, CurrentPlayer, _,Mode], [Move|Rest], MaxValue, Temp, Moves) :-
    color(CurrentPlayer, Colour),
    opponent(Colour, Opponent),
    color(CurrentOpponent, Opponent),
    move([Board, CurrentOpponent, _,Mode], Move, NewGameState),
    value(NewGameState, Opponent, 10000),
    9000 > MaxValue,    % slightly worst than winning so it still chooses the win move when possible
    best_moves([Board, CurrentPlayer, _,Mode], Rest, 9000, [Move], Moves).

%the value of the new play is greater than the max score so it is now the best move
best_moves([Board, CurrentPlayer, _,Mode], [Move|Rest], MaxValue, Temp, Moves) :-
    color(CurrentPlayer, Colour),
    opponent(Colour, Opponent),
    move([Board, CurrentPlayer, _,Mode], Move, NewGameState),
    value(NewGameState, Colour, Value),
    potential_score([Board, _, _, Mode], Opponent, OpponentScore1),   %get opponents potential_score before we make our move
    potential_score(NewGameState, Opponent, OpponentScore2),        %get opponents potential_score after we make our move
    BlockValue2 is OpponentScore1 - OpponentScore2,                  % good if we were able to block opponents pieces
    BlockValue is BlockValue2 * 2,
    TotalValue is Value + BlockValue,
    TotalValue > MaxValue,
    best_moves([Board, CurrentPlayer, _,Mode], Rest, TotalValue, [Move], Moves).

%the value of the new play is less than the max score so we skip this one
best_moves([Board, CurrentPlayer, _,Mode], [Move|Rest], MaxValue, Temp, Moves) :-
    color(CurrentPlayer, Colour),
    opponent(Colour, Opponent),
    move([Board, CurrentPlayer, _,Mode], Move, NewGameState),
    value(NewGameState, Colour, Value),
    potential_score([Board, _, _, Mode], Opponent, OpponentScore1),   %get opponents potential_score before we make our move
    potential_score(NewGameState, Opponent, OpponentScore2),        %get opponents potential_score after we make our move
    BlockValue2 is OpponentScore1 - OpponentScore2,                  % good if we were able to block opponents pieces
    BlockValue is BlockValue2 * 2,
    TotalValue is Value + BlockValue,
    TotalValue < MaxValue,
    best_moves([Board, CurrentPlayer, _,Mode], Rest, MaxValue, Temp, Moves).

%the value of the new play is the same as the max score so we add the move to the list
best_moves([Board, CurrentPlayer, _,Mode], [Move|Rest], MaxValue, Temp, Moves) :-
    color(CurrentPlayer, Colour),
    opponent(Colour, Opponent),
    move([Board, CurrentPlayer, _,Mode], Move, NewGameState),
    value(NewGameState, Colour, Value),
    potential_score([Board, _, _, Mode], Opponent, OpponentScore1),   %get opponents potential_score before we make our move
    potential_score(NewGameState, Opponent, OpponentScore2),        %get opponents potential_score after we make our move
    BlockValue2 is OpponentScore1 - OpponentScore2,                  % good if we were able to block opponents pieces
    BlockValue is BlockValue2 * 2,
    TotalValue is Value + BlockValue,
    TotalValue =:= MaxValue,
    best_moves([Board, CurrentPlayer, _,Mode], Rest, MaxValue, [Move | Temp], Moves).

/* calculate the progression score based on how the pieces occupy the goal edges*/
progression_score([Board, _, _,_], b, Score):-
    progression_score_calc(Board, b, Score).
progression_score([Board, _, _,_], w, Score) :-
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
potential_score([Board, _, _,Mode], Colour, TotalScore) :-
    all_pieces(Board, Colour, Pieces),
    potential_score_calc([Board, _, _,Mode], Pieces, Colour, 0, TotalScore).

%no more pieces to explore
potential_score_calc(_, [], _, Score, Score).

%these are part of an effort to balance the point system in value
% surrounding less than 3 so it doesnt bring that much value
potential_score_calc([Board, _, _, Mode], [Row-Col | Rest], Colour, Score, TotalScore) :-
    length(Board, Size), 
    neighbor_coords(Row, Col, Size, Neighbors),
    get_surrounding_moves([Board, _, _,Mode], Colour, Neighbors, [], Surrounding),
    length(Surrounding, SurLength),
    SurLength < 3,
    SurLength2 is SurLength / 2,
    NewScore is SurLength2 + Score,
    potential_score_calc([Board, _, _, Mode], Rest, Colour, NewScore, TotalScore).
% surrounding more than 2, add it to the other potential scores
potential_score_calc([Board, _, _, Mode], [Row-Col | Rest], Colour, Score, TotalScore) :-
    length(Board, Size), 
    neighbor_coords(Row, Col, Size, Neighbors),
    get_surrounding_moves([Board, _, _,Mode], Colour, Neighbors, [], Surrounding),
    length(Surrounding, SurLength),
    SurLength > 2,
    NewScore is SurLength + Score,
    potential_score_calc([Board, _, _, Mode], Rest, Colour, NewScore, TotalScore).

/* calculate the score based on the connection it is making and if it is moving in the right direction*/
connection_score([Board, _, _, _], Colour, TotalScore) :-
    length(Board, Size),
    all_pieces(Board, Colour, AllPieces),
    connection_score_calc(Board, Colour, Size, AllPieces, [], 0, TotalScore).

connection_score_calc(_, _, _, [], _, Score, Score). % no more pieces to explore
connection_score_calc(Board, Colour, Size, [Row-Col | Rest], Visited, Score, FinalScore) :-
    \+ member(Row-Col, Visited),  %in this case visit is probably redundant but i will keep it for safekeeping
    neighbor_coords(Row, Col, Size, Neighbors),
    get_surrounding_stones(Board, Colour, Neighbors, Surrounding),
    neighbors_calc(Row, Surrounding, 0, NeiScore),
    S1 is Score + NeiScore,
    connection_score_calc(Board, Colour, Size, Rest, [Row-Col | Visited], S1, FinalScore).
connection_score_calc(Board, Colour, Size, [Row-Col | Rest], Visited, Score, FinalScore) :-
    member(Row-Col, Visited),
    connection_score_calc(Board, Colour, Size, Rest, UpdatedVisited, Score, FinalScore).

%this will apply 100 per connection towards the goal and 20 per sidetracking
neighbors_calc(_, [], Score, Score). %base case no more stones
neighbors_calc(Row, [NeiRow-_ | Rest],Score, FinalScore):-
    RowDiff is NeiRow - Row,
    RowDiff =:= 0, %no change in Row
    NewScore is Score + 1,
    neighbors_calc(Row, Rest, NewScore, FinalScore).
neighbors_calc(Row, [NeiRow-_ | Rest],Score, FinalScore):-
    RowDiff1 is NeiRow - Row,
    abs(RowDiff1, RowDiff),
    RowDiff =:= 1, %goes towards the goal
    NewScore is Score + 3,
    neighbors_calc(Row, Rest, NewScore, FinalScore).

/*
% calculate the score based on the different paths it has, how big they are and if they move optimally in the right direction
% using a dfs to navigate every path and calculate the score based on the direction the path is making, rewarding that moving towards the goal
% function working perfectly but having weird interaction whith best_moves so we ditched it
connection_score([Board, _, _,_], Colour, TotalScore) :-
    length(Board, Size),
    all_pieces(Board, Colour, AllPieces),
    connection_score_calc(Board, Colour, Size, AllPieces, [], AllScores),
    list_sum(AllScores, TotalScore).

connection_score_calc(_, _, _, [], _, []). % no more pieces to explore
connection_score_calc(Board, Colour, Size, [Row-Col | Rest], Visited, [Score | Scores]) :-
    \+ member(Row-Col, Visited),
    dfs_score(Board, Row-Col, Row-Col, Size, [], VisitedAfterDFS, Colour, 0.0, ScoreRaw),
    abs(ScoreRaw, Score),
    append(Visited, VisitedAfterDFS, UpdatedVisited),
    connection_score_calc(Board, Colour, Size, Rest, UpdatedVisited, Scores).
connection_score_calc(Board, Colour, Size, [Row-Col | Rest], Visited, Scores) :-
    member(Row-Col, Visited),
    connection_score_calc(Board, Colour, Size, Rest, UpdatedVisited, Scores).

dfs_score(_, _, Row-Col, _, Visited, Visited, _, CurrentScore, CurrentScore) :-
    member(Row-Col, Visited). % If already visited, return the current score.

% visit the piece
dfs_score(Board, OldRow-OldCol, Row-Col, Size, Visited, SuperUpdatedVisited, Colour, CurrentScore, FinalScore) :- % UpdatedVisited is basically Visited but one step further and it is used to tell connection_score which pieces were visited
    \+ member(Row-Col, Visited),
    neighbor_coords(Row, Col, Size, Neighbors),            % Get orthogonal neighbors
    score_calculator(OldRow, Row, CurrentScore, NewScore),
    UpdatedVisited = [Row - Col | Visited], %new Visited List
    explore_neighbors_score(Board, Neighbors, Size, UpdatedVisited, SuperUpdatedVisited, Colour, Row-Col, NewScore, FinalScore).

% Base case: No more neighbors to explore.
explore_neighbors_score(_, [], _, Visited, Visited, _, _, AccumulatedScore, AccumulatedScore). 
explore_neighbors_score(Board, [NextRow-NextCol | Rest], Size, Visited, SuperUpdatedVisited, Colour, Row-Col, CurrentScore, FinalScore) :-
    \+ member(NextRow-NextCol, Visited),
    nth1(NextRow, Board, RowList),
    nth1(NextCol, RowList, Colour), % visit point only if it has a piece of correct colour
    dfs_score(Board, Row-Col, NextRow-NextCol, Size, Visited, UpdatedVisited, Colour, CurrentScore, NeighborScore), % Continue DFS
    explore_neighbors_score(Board, Rest, Size, UpdatedVisited, SuperUpdatedVisited, Colour, Row-Col, NeighborScore, FinalScore). % Process the rest of the neighbors of the piece at Row-Col

%if a neighbour doesnt have the right stone in it continue
explore_neighbors_score(Board, [NextRow-NextCol | Rest], Size, Visited, SuperUpdatedVisited, Colour, Row-Col, CurrentScore, FinalScore) :-
    nth1(NextRow, Board, RowList),
    \+ nth1(NextCol, RowList, Colour), % visit point only if it has a piece of correct colour
    explore_neighbors_score(Board, Rest, Size, Visited, SuperUpdatedVisited, Colour, Row-Col, CurrentScore, FinalScore). % Process the rest of the neighbors of the piece at Row-Col

%if a neighbor was already visited, skip it
explore_neighbors_score(Board, [NextRow-NextCol | Rest], Size, Visited, SuperUpdatedVisited, Colour, Row-Col, CurrentScore, FinalScore) :-
    member(NextRow-NextCol, Visited),
    explore_neighbors_score(Board, Rest, Size, Visited, SuperUpdatedVisited, Colour, Row-Col, CurrentScore, FinalScore). % Process the rest of the neighbors of the piece at Row-Col


% add 1 when moving downwards
score_calculator(OldRow, NextRow, OldScore, NewScore) :-
    RowDiff is NextRow - OldRow,
    RowDiff =:= 1,
    NewScore is OldScore + 100.

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
*/
/* auxiliary stuff */

%check if a stone is completely blocked and cannot create any connection because no cordinates near it are valid
is_blocked_completely(Board, Row-Col, Size, Colour) :-
    neighbor_coords(Row, Col, Size, Neighbors),
    get_surrounding_moves(Board, Colour, Neighbors, [], Surrounding),
    Surrounding = []. %it is blocked completely if it has no valid moves around it

% get valid stones from a list of neighboring coordinates
get_surrounding_stones(_, _, [], []). % no more neighbors
% add if it is from correct Colour
get_surrounding_stones(Board, Colour, [Row-Col|Rest],[Row-Col | Surrounding]) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Colour),
    get_surrounding_stones(Board, Colour, Rest, Surrounding).
get_surrounding_stones(Board, Colour, [Row-Col|Rest],Surrounding) :-
    nth1(Row, Board, RowList),
    \+ nth1(Col, RowList, Colour),
    get_surrounding_stones(Board, Colour, Rest, Surrounding).

% get valid moves from a list of neighboring coordinates
get_surrounding_moves(_, _, [], Surrounding, Surrounding). % no more neighbors
% add if valid
get_surrounding_moves([Board, _, _,Mode | _], Colour, [Row-Col|Rest], Sur, Surrounding) :-
    is_valid_move(Board, Row, Col, Colour, Mode),
    Sur1 = [Row-Col|Sur],
    get_surrounding_moves([Board, _, _,Mode | _], Colour, Rest, Sur1, Surrounding).
%skip if not valid
get_surrounding_moves([Board, _, _,Mode | _], Colour, [Row-Col|Rest], Sur, Surrounding) :-
    \+ is_valid_move(Board, Row, Col, Colour, Mode),
    get_surrounding_moves([Board, _, _,Mode | _], Colour, Rest, Sur, Surrounding).

% to check if a list is a subset of another list
list_subset([], _). % Base case: empty list is always a subset
list_subset([X|Rest], List2) :-
    select(X, List2, ReducedList2), % Remove one occurrence of X from List2
    list_subset(Rest, ReducedList2). % Recursively check the rest

%get the coordinates to all of the colours pieces as Rou-Column
all_pieces(Board, Colour, Pieces) :-
    findall(
        Row-Col,
        (   nth1(Row, Board, RowList),
            nth1(Col, RowList, Colour)),
        Pieces
    ).