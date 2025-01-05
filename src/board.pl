
% Define the board
board_middle_state([
    [w, empty, b, empty, w, empty, empty, b, empty, w],
    [empty, w, empty, empty, b, empty, w, empty, empty, empty],
    [b, empty, empty, w, empty, empty, empty, w, b, empty],
    [empty, b, empty, empty, empty, b, empty, empty, empty, w],
    [empty, empty, w, empty, b, w, empty, empty, b, empty],
    [w, empty, empty, empty, w, empty, b, empty, empty, empty],
    [empty, b, w, empty, empty, empty, w, empty, w, empty],
    [b, empty, empty, b, empty, w, empty, empty, empty, w],
    [empty, empty, empty, empty, empty, empty, b, empty, b, empty],
    [w, empty, b, empty, empty, empty, empty, w, empty, b]
]).
board_black_win([
    [w, empty, b, b, w, w, empty, b, empty, w],
    [w, w, empty, b, b, empty, w, empty, empty, empty],
    [b, w, b, w, empty, empty, empty, w, b, empty],
    [w, b, b, empty, empty, b, empty, empty, empty, w],
    [w, b, w, w, b, w, empty, empty, b, empty],
    [w, b, b, empty, w, empty, b, empty, empty, empty],
    [w, b, w, empty, empty, empty, w, empty, w, empty],
    [b, b, w, b, empty, w, empty, empty, empty, w],
    [w, b, b, empty, empty, empty, b, empty, b, empty],
    [w, w, b, empty, empty, empty, empty, w, empty, b]
]).