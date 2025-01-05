
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
    [w, w, b, b, w, w, empty, b, empty, w],
    [w, w, b, b, b, empty, w, empty, empty, empty],
    [b, w, b, w, empty, empty, empty, w, b, empty],
    [w, b, b, empty, empty, b, empty, empty, empty, w],
    [w, b, w, w, b, w, empty, empty, b, empty],
    [w, b, b, empty, w, empty, b, empty, empty, empty],
    [w, b, w, empty, empty, empty, w, empty, w, empty],
    [b, b, w, b, empty, w, empty, empty, empty, w],
    [w, b, b, empty, empty, empty, b, empty, b, empty],
    [w, empty, empty, empty, empty, empty, empty, w, empty, b]
]).
board_white_win([
    [w, empty, b, empty, w, empty, empty, b, empty, w],
    [empty, w, b, empty, b, b, w, b, empty, empty],
    [b, empty, empty, w, b, empty, b, w, b, empty],
    [b, b, empty, empty, b, b, w, empty, empty, w],
    [w, w, w, w, b, w, w, w, b, empty],
    [w, b, b, w, w, w, b, w, w, empty],
    [empty, b, w, b, empty, empty, w, empty, w, empty],
    [b, empty, empty, b, empty, w, empty, empty, empty, w],
    [empty, empty, empty, empty, empty, empty, b, empty, b, empty],
    [w, empty, b, empty, empty, empty, empty, w, empty, b]
]).
board_draw([
    [w, b, b, w, w, b, w, b, w, w],
    [b, w, b, b, w, w, b, w, b, b],
    [b, b, w, w, b, w, b, b, w, w],
    [w, b, b, w, b, b, w, b, w, b],
    [b, w, w, b, w, w, b, w, b, b],
    [w, b, b, w, b, w, b, b, w, w],
    [b, w, w, b, w, b, w, b, w, b],
    [w, b, b, w, b, w, b, w, b, w],
    [b, w, w, b, w, b, w, b, w, b],
    [w, b, b, w, w, b, w, empty, b, w]
]).
