:- ensure_loaded('utils.pl').

:- dynamic board/2.
:- dynamic gold_tile/2.
:- dynamic line/3.
:- dynamic board_size/2.

% board(+State, -Board)
% Unifies Board with the board at the current State for starting a game or for demonstrating different board states
board(initial, 
[
    position(cian-circle-1, tile(3, 0)),
    position(cian-circle-2, tile(1, 1)),
    position(cian-square-1, tile(2, 1)),
    position(cian-triangle-1, tile(3, 1)),
    position(cian-square-2, tile(4, 1)),
    position(cian-circle-3, tile(5, 1)),
    position(cian-triangle-2, tile(2, 2)),
    position(cian-pentagon-1, tile(3, 2)),
    position(cian-triangle-3, tile(4, 2)),
    position(cian-circle-4, tile(1, 3)),
    position(cian-square-3, tile(3, 3)),
    position(cian-circle-5, tile(5, 3)),
    position(cian-circle-6, tile(3, 4)),
    position(red-circle-1, tile(3, 6)),
    position(red-circle-2, tile(1, 7)),
    position(red-triangle-1, tile(2, 7)),
    position(red-square-1, tile(3, 7)),
    position(red-triangle-2, tile(4, 7)),
    position(red-circle-3, tile(5, 7)),
    position(red-square-2, tile(2, 8)),
    position(red-pentagon-1, tile(3, 8)),
    position(red-square-3, tile(4, 8)),
    position(red-circle-4, tile(1, 9)),
    position(red-triangle-3, tile(3, 9)),
    position(red-circle-5, tile(5, 9)),
    position(red-circle-6, tile(3, 10))
]
).

board(intermediate, 
[
    position(cian-circle-1, tile(4, 5)),
    position(cian-square-1, tile(5, 7)),
    position(cian-triangle-1, tile(5,2)),
    position(cian-circle-3, tile(5, 1)),
    position(cian-pentagon-1, tile(3, 0)),
    position(cian-triangle-3, tile(0, 5)),
    position(cian-circle-6, tile(5, 5)),
    position(red-circle-1, tile(3, 2)),
    position(red-square-1, tile(5, 6)),
    position(red-pentagon-1, tile(3, 5)),
    position(red-circle-4, tile(2, 3)),
    position(red-triangle-1, tile(3, 9)),
    position(red-circle-6, tile(1, 5))
]
).

board(final, 
[
    position(cian-circle-1, tile(5, 1)),
    position(cian-pentagon-1, tile(5, 6)),
    position(red-circle-1, tile(3, 5))
]
).

% tile(+X, +Y)
% This predicate is true if there is a tile at coordinates X, Y on the board.
tile(X, Y) :- 
    line(Y, MinX, MaxX),
    between(MinX, MaxX, X).

% gold_tile(+X, +Y)
% Position of the gold tiles on the board
gold_tile(1, 5).
gold_tile(5, 5).

% line(+Y, -MinX, -MaxX)
% Unifies MinX and MaxX with the minimum and maximum X values of the line Y
line(0, 2, 4).
line(1, 1, 5).
line(2, 1, 5).
line(3, 1, 5).
line(4, 0, 6).
line(5, 0, 6).
line(6, 1, 5).
line(7, 1, 5).	
line(8, 1, 5).
line(9, 1, 5).
line(10, 3, 3).

% board_size(-X, -Y)
% Unifies X and Y with the size of the board (X is the number of columns and Y is the number of lines)
board_size(7, 11).

% movement(+Piece, -Movement)
% Unifies Movement with the number of tiles that Piece can move in one turn (Excluding additional movements from the gold tiles)
movement(circle, 1).
movement(triangle, 3).
movement(square, 4).
movement(pentagon, 5).

% player(+Player)
% All players
player(cian).
player(red).

% other_player(+Player, -OtherPlayer)
% Unifies OtherPlayer with the opponent of Player
other_player(cian, red).
other_player(red, cian).

% piece_info(+Piece, -Player, -Type)
% Unifies Player and Type with the player and type of a Piece
piece_info(Player-Type-_, Player, Type).

% piece_print_info(+Type, +Player, -PrintType)
% Unifies PrintType with the text of Piece to be printed in the board
piece_print_info(circle, cian, 'CC').
piece_print_info(triangle, cian, 'CT').
piece_print_info(square, cian, 'CS').
piece_print_info(pentagon, cian, 'CP').
piece_print_info(circle, red, 'RC').
piece_print_info(triangle, red, 'RT').
piece_print_info(square, red, 'RS').
piece_print_info(pentagon, red, 'RP').

% adjacent(tile(+X, +Y), tile(?X1, ?Y1))
% Unifies tile(X1, Y1) with a tile adjacent to tile(X, Y) or verifies if tile(X1, Y1) is adjacent to tile(X, Y)
% Adjacent tiles are tiles that are next to each other, including diagonally. The vector for adjacent tiles is
% [1, 0], [-1, 0], [0, 1], [0, -1], [1, -1], [-1, -1] when X is odd and
% [1, 0], [-1, 0], [0, 1], [0, -1], [1, 1], [-1, 1] when X is even 
adjacent(tile(X, Y), tile(X1, Y)) :- 
    tile(X, Y),
    tile(X1, Y),
    DIFX is X - X1,
    abs(DIFX, ABSX),
    ABSX == 1.

adjacent(tile(X, Y), tile(X, Y1)) :- 
    tile(X, Y),
    tile(X, Y1),
    DIFY is Y - Y1,
    abs(DIFY, ABSY),
    ABSY == 1.

adjacent(tile(X, Y), tile(X1, Y1)) :-
    1 is X mod 2,
    tile(X, Y),
    tile(X1, Y1),
    DIFY is Y - Y1,
    DIFY == 1,
    DIFX is X1 - X,
    abs(DIFX, ABSX),
    ABSX == 1.

adjacent(tile(X, Y), tile(X1, Y1)) :-
    0 is X mod 2,
    tile(X, Y),
    tile(X1, Y1),
    DIFY is Y - Y1,
    DIFY == -1,
    DIFX is X1 - X,
    abs(DIFX, ABSX),
    ABSX == 1.

% evenq_to_cube(+X, +Y, -cube(-Q, -R, -S))
% Transforms the even-q coordinates X, Y into cube coordinates Q, R, S
evenq_to_cube(X, Y, cube(Q, R, S)) :-
    Q is X,
    R is Y - (X + (X mod 2)) div 2,
    S is -Q - R,
    !.

% distance(tile(+X, +Y), tile(+X1, +Y1), -Distance)
% Unifies Distance with the calculated distance between tile(X, Y) and tile(X1, Y1)
distance(tile(X, Y), tile(X1, Y1), Distance) :-
    evenq_to_cube(X, Y, cube(QC, RC, SC)),
    evenq_to_cube(X1, Y1, cube(QC1, RC1, SC1)),
    Distance is (abs(QC - QC1) + abs(RC - RC1) + abs(SC - SC1)) div 2,
    !.

% find_piece(+Board, ?Piece, ?Tile)
% Unifies Piece with the piece at Tile on Board
find_piece(Board, Piece, tile(X, Y)) :-
    member(position(Piece, tile(X, Y)), Board).

% tile_to_string(+Board, +Tile, -String)
% Unifies String with the string representation of Tile on Board, none if there is no piece on Tile
tile_to_string(Board, tile(X, Y), String) :-
    find_piece(Board, Piece, tile(X, Y)),
    piece_info(Piece, Player, Type),
    piece_print_info(Type, Player, String),
    !.

tile_to_string(_, tile(X, Y), String) :-
    gold_tile(X, Y),
    String = 'GT',
    !.

tile_to_string(_, tile(_, _), 'none').

% ------------------------- %
%         DRAW BOARD        %
% ------------------------- %

% draw_x_line\0
% Draws the X coordinates of the board 
draw_x_line :-
    board_size(X, Y),
    number_of_digits(Y, Digits),
    draw_x_line(X, Digits).
draw_x_line(X, Digits) :-
    Y_Size is Digits + 1,
    print_n(Y_Size, ' '),
    write('|X |'),
    draw_x_line_aux(X, 0),
    write('  |'), nl.

% draw_x_line_aux(+X, +CurrentX)
% Draws the X coordinates of the board 
draw_x_line_aux(X, X) :-
    !.
draw_x_line_aux(X, CurrentX) :-
    format('X~d|', [CurrentX]),
    NextX is CurrentX + 1,
    draw_x_line_aux(X, NextX).

% draw_hyphen_line\0
% Draws a separator line of the board
draw_hyphen_line :-
    board_size(X, Y),
    number_of_digits(Y, Digits),
    Y_Size is Digits + 1,
    X_Size is 3*X + 5,
    print_n(Y_Size, '-'),
    write('|'),
    print_n(X_Size, '-'),
    write('|'),
    print_n(Y_Size, '-'),
    nl.

% draw_header\0
% Draws the header of the board
draw_header :-
    draw_x_line,
    draw_hyphen_line.

% draw_footer\0
% Draws the footer of the board
draw_footer :-
    draw_hyphen_line,
    draw_x_line.

% draw_first_line(+Board)
% Draws the first line of the board
draw_first_line(Board) :-
    board_size(_, Y),
    number_of_digits(Y, Digits),
    write('Y'),
    print_n(Digits, ' '),
    write('|'),
    build_line(Board, -1, -1, Line),
    draw_hexagons(Line, draw(start, _)),
    write('|Y'),
    nl.

% draw_board(+Board)
% Draws the board
draw_board(Board) :-
    board_size(_, Y),
    MaxY is 2*Y - 1,
    draw_header,
    draw_first_line(Board),
    draw_board_aux(Board, 0, MaxY),
    draw_footer.

% draw_board_aux(+Board, +Y, +MaxY)
% Draws the board from Y to MaxY, with the borders, assuming each "line of the real board" is 2 lines of the printed board
draw_board_aux(_, Y, MaxY) :- 
    Y > MaxY,
    !.

draw_board_aux(Board, Y, MaxY) :-
    draw_board_line(Board, Y),
    Y1 is Y + 1,
    !,
    draw_board_aux(Board, Y1, MaxY).

% draw_board_line(+Board, +Y)
% Draws the line Y of the board, assuming each "line of the real board" is 2 lines of the printed board
draw_board_line(Board, Y) :-
    CurrentY is Y // 2,
    CurrentY < 10,
    !,
    format('Y~d |', [CurrentY]),
    build_line(Board, CurrentY, Y, Line),
    draw_hexagons(Line, draw(start, _)),
    format('|Y~d', [CurrentY]),
    nl.

draw_board_line(Board, Y) :-
    CurrentY is Y // 2,
    format('Y~d|', [CurrentY]),
    build_line(Board, CurrentY, Y, Line),
    draw_hexagons(Line, draw(start, _)),
    format('|Y~d', [CurrentY]),
    nl.

% build_line(+Board, +CurrentY, +Y, -Line)
% Builds a Line, which is a list of draw predicates that represent a part of line Y of the board
build_line(Board, CurrentY, Y, Line) :-
    build_line(Board, CurrentY, Y, 0, [draw(start, _)], Line).

% build_line(+Board, +CurrentY, +Y, +X, +Aux, -Line)
% Builds a Line, which is a list of draw predicates that represent a part of line Y of the board
build_line(_, _, _, X, Aux, Line) :-
    board_size(X, _),
    append(Aux, [draw(none, _)], Line),
    !.

build_line(Board, CurrentY, Y, X, Aux, Line) :-
    \+ tile(X, CurrentY),
    1 is Y mod 2,
    1 is X mod 2,
    NY is CurrentY + 1,
    tile(X, NY),
    append(Aux, [draw(startBottom, _)], Aux1),
    X1 is X + 1,
    !,
    build_line(Board, CurrentY, Y, X1, Aux1, Line).

build_line(Board, CurrentY, Y, X, Aux, Line) :-
    \+ tile(X, CurrentY),
    0 is Y mod 2,
    0 is X mod 2,
    PY is CurrentY - 1,
    tile(X, PY),
    append(Aux, [draw(bottom, _)], Aux1),
    X1 is X + 1,
    !,
    build_line(Board, CurrentY, Y, X1, Aux1, Line).

build_line(Board, CurrentY, Y, X, Aux, Line) :-
    \+ tile(X, CurrentY),
    append(Aux, [draw(none, _)], Aux1),
    X1 is X + 1,
    !,
    build_line(Board, CurrentY, Y, X1, Aux1, Line).

build_line(Board, CurrentY, Y, X, Aux, Line) :-
    0 is Y mod 2,
    0 is X mod 2,
    !,
    PY is CurrentY - 1,
    (
        tile(X, PY) -> append(Aux, [draw(bottom, _)], Aux1) ;
        append(Aux, [draw(startBottom, _)], Aux1)
        ),
    X1 is X + 1,
    build_line(Board, CurrentY, Y, X1, Aux1, Line).

build_line(Board, CurrentY, Y, X, Aux, Line) :-
    0 is Y mod 2,
    1 is X mod 2,
    !,
    tile_to_string(Board, tile(X, CurrentY), String),
    append(Aux, [draw(top, String)], Aux1),
    X1 is X + 1,
    build_line(Board, CurrentY, Y, X1, Aux1, Line).

build_line(Board, CurrentY, Y, X, Aux, Line) :-
    1 is Y mod 2,
    0 is X mod 2,
    !,
    tile_to_string(Board, tile(X, CurrentY), String),
    append(Aux, [draw(top, String)], Aux1),
    X1 is X + 1,
    build_line(Board, CurrentY, Y, X1, Aux1, Line).

build_line(Board, CurrentY, Y, X, Aux, Line) :-
    1 is Y mod 2,
    1 is X mod 2,
    !,
    append(Aux, [draw(bottom, _)], Aux1),
    X1 is X + 1,
    build_line(Board, CurrentY, Y, X1, Aux1, Line).

% draw_hexagons(+Line, +LastState)
% Draws the hexagons of a line, and updates the LastState
draw_hexagons([], _) :- !.

draw_hexagons([H|T], LastState) :-
    draw_hexagon(LastState, H),
    draw_hexagons(T, H).

% draw_hexagon(+LastState, +State)
% Logic for drawing a hexagon, depending on the LastState and the State
draw_hexagon(draw(top, _), draw(none, _)) :-
    write('\\  '),
    !.

draw_hexagon(draw(bottom, _), draw(none, _)) :-
    write('/  '),
    !.

draw_hexagon(_, draw(none, _)) :-
    write('   '),
    !.

draw_hexagon(_, draw(start, _)) :-
    write('  '),
    !.

draw_hexagon(_, draw(bottom, _)) :-
    write('\\__'),
    !.

draw_hexagon(_, draw(top, none)) :-
    write('/  '),
    !.
draw_hexagon(_, draw(top, PrintType)) :-
    format('/~w', [PrintType]),
    !.

draw_hexagon(draw(top, _), draw(startBottom, _)) :-
    write('\\__'),
    !.

draw_hexagon(_, draw(startBottom, _)) :-
    write(' __'),
    !.

% ------------------------------------ %
%         VARIABLE SIZED BOARDS        %
% ------------------------------------ %

% clear_board\0
% Clears the board
clear_board :-
    retractall(board(_, _)),
    retractall(gold_tile(_, _)),
    retractall(line(_, _, _)),
    retractall(board_size(_, _)).

% assert_list(+List)
% Asserts all elements of List
assert_list([]).
assert_list([H|T]) :-
    assert(H),
    assert_list(T).

% create_new_board(+Size)
% Creates a new board with Size lines, with a default number of columns for that size
create_new_board(11) :-
    clear_board,
    assert(board_size(7, 11)),
    assert_list([
        line(0, 2, 4),
        line(1, 1, 5),
        line(2, 1, 5),
        line(3, 1, 5),
        line(4, 0, 6),
        line(5, 0, 6),
        line(6, 1, 5),
        line(7, 1, 5),	
        line(8, 1, 5),
        line(9, 1, 5),
        line(10, 3, 3)
    ]),
    assert_list([
        gold_tile(1, 5),
        gold_tile(5, 5)
    ]),
    assert(board(initial, 
    [
        position(cian-circle-1, tile(3, 0)),
        position(cian-circle-2, tile(1, 1)),
        position(cian-square-1, tile(2, 1)),
        position(cian-triangle-1, tile(3, 1)),
        position(cian-square-2, tile(4, 1)),
        position(cian-circle-3, tile(5, 1)),
        position(cian-triangle-2, tile(2, 2)),
        position(cian-pentagon-1, tile(3, 2)),
        position(cian-triangle-3, tile(4, 2)),
        position(cian-circle-4, tile(1, 3)),
        position(cian-square-3, tile(3, 3)),
        position(cian-circle-5, tile(5, 3)),
        position(cian-circle-6, tile(3, 4)),
        position(red-circle-1, tile(3, 6)),
        position(red-circle-2, tile(1, 7)),
        position(red-triangle-1, tile(2, 7)),
        position(red-square-1, tile(3, 7)),
        position(red-triangle-2, tile(4, 7)),
        position(red-circle-3, tile(5, 7)),
        position(red-square-2, tile(2, 8)),
        position(red-pentagon-1, tile(3, 8)),
        position(red-square-3, tile(4, 8)),
        position(red-circle-4, tile(1, 9)),
        position(red-triangle-3, tile(3, 9)),
        position(red-circle-5, tile(5, 9)),
        position(red-circle-6, tile(3, 10))
    ])),
    assert(board(intermediate, 
    [
        position(cian-circle-1, tile(4, 5)),
        position(cian-square-1, tile(5, 7)),
        position(cian-triangle-1, tile(5,2)),
        position(cian-circle-3, tile(5, 1)),
        position(cian-pentagon-1, tile(3, 0)),
        position(cian-triangle-3, tile(0, 5)),
        position(cian-circle-6, tile(5, 5)),
        position(red-circle-1, tile(3, 2)),
        position(red-square-1, tile(5, 6)),
        position(red-pentagon-1, tile(3, 5)),
        position(red-circle-4, tile(2, 3)),
        position(red-triangle-1, tile(3, 9)),
        position(red-circle-6, tile(1, 5))
    ])),
    assert(board(final, 
    [
        position(cian-circle-1, tile(5, 1)),
        position(cian-pentagon-1, tile(5, 6)),
        position(red-circle-1, tile(3, 5))
    ])).

create_new_board(13) :-
    clear_board,
    assert(board_size(8, 13)),
    assert_list([
        line(0, 2, 5),
        line(1, 1, 6),
        line(2, 1, 6),
        line(3, 1, 6),
        line(4, 1, 6),
        line(5, 0, 7),
        line(6, 0, 7),
        line(7, 1, 6),	
        line(8, 1, 6),
        line(9, 1, 6),
        line(10, 1, 6),
        line(11, 1, 6),
        line(12, 3, 3)
    ]),
    assert_list([
        gold_tile(1, 6),
        gold_tile(7, 6)
    ]),
    assert(board(initial, 
    [
        position(cian-circle-1, tile(4, 0)),
        position(cian-circle-2, tile(2, 1)),
        position(cian-square-1, tile(3, 1)),
        position(cian-triangle-1, tile(4, 1)),
        position(cian-square-2, tile(5, 1)),
        position(cian-circle-3, tile(6, 1)),
        position(cian-triangle-2, tile(3, 2)),
        position(cian-pentagon-1, tile(4, 2)),
        position(cian-triangle-3, tile(5, 2)),
        position(cian-circle-4, tile(3, 3)),
        position(cian-square-3, tile(4, 3)),
        position(cian-circle-5, tile(5, 3)),
        position(cian-circle-6, tile(4, 4)),
        position(red-circle-1, tile(4, 8)),
        position(red-circle-2, tile(2, 9)),
        position(red-triangle-1, tile(3, 9)),
        position(red-square-1, tile(4, 9)),
        position(red-triangle-2, tile(5, 9)),
        position(red-circle-3, tile(6, 9)),
        position(red-square-2, tile(3, 10)),
        position(red-pentagon-1, tile(4, 10)),
        position(red-square-3, tile(5, 10)),
        position(red-circle-4, tile(3, 11)),
        position(red-triangle-3, tile(4, 11)),
        position(red-circle-5, tile(5, 11)),
        position(red-circle-6, tile(4, 7))
    ])),
    assert(board(intermediate, 
    [
        position(cian-circle-1, tile(5, 6)),
        position(cian-square-1, tile(6, 8)),
        position(cian-triangle-1, tile(6,3)),
        position(cian-circle-3, tile(6, 2)),
        position(cian-pentagon-1, tile(4, 1)),
        position(cian-triangle-3, tile(1, 6)),
        position(cian-circle-6, tile(3, 6)),
        position(red-circle-1, tile(4, 3)),
        position(red-square-1, tile(6, 7)),
        position(red-pentagon-1, tile(4, 6)),
        position(red-circle-4, tile(3, 4)),
        position(red-triangle-1, tile(4, 12)),
        position(red-circle-6, tile(2, 6))
    ])),
    assert(board(final, 
    [
        position(cian-circle-1, tile(6, 2)),
        position(cian-pentagon-1, tile(6, 7)),
        position(red-circle-1, tile(4, 6))
    ])).


% Board cant have more than 10 columns
create_new_board(15) :-
    clear_board,
    assert(board_size(10, 15)),
    assert_list([
        line(0, 2, 7),
        line(1, 1, 8),
        line(2, 1, 8),
        line(3, 1, 8),
        line(4, 1, 8),
        line(5, 1, 8),
        line(6, 0, 9),
        line(7, 0, 9),	
        line(8, 1, 8),
        line(9, 1, 8),
        line(10, 1, 8),
        line(11, 1, 8),
        line(12, 1, 8),
        line(13, 2, 7),
        line(14, 3, 7)
    ]),
    assert_list([
        gold_tile(1, 7),
        gold_tile(9, 7)
    ]),
    assert(board(initial, 
    [
        position(cian-circle-1, tile(5, 1)),
        position(cian-circle-2, tile(3, 2)),
        position(cian-square-1, tile(4, 2)),
        position(cian-triangle-1, tile(5, 2)),
        position(cian-square-2, tile(6, 2)),
        position(cian-circle-3, tile(7, 2)),
        position(cian-triangle-2, tile(4, 3)),
        position(cian-pentagon-1, tile(5, 3)),
        position(cian-triangle-3, tile(6, 3)),
        position(cian-circle-4, tile(3, 4)),
        position(cian-square-3, tile(5, 4)),
        position(cian-circle-5, tile(7, 4)),
        position(cian-circle-6, tile(5, 5)),
        position(red-circle-1, tile(5, 9)),
        position(red-circle-2, tile(3, 10)),
        position(red-triangle-1, tile(4, 10)),
        position(red-square-1, tile(5, 10)),
        position(red-triangle-2, tile(6, 10)),
        position(red-circle-3, tile(7, 10)),
        position(red-square-2, tile(4, 11)),
        position(red-pentagon-1, tile(5, 11)),
        position(red-square-3, tile(6, 11)),
        position(red-circle-4, tile(3, 12)),
        position(red-triangle-3, tile(5, 12)),
        position(red-circle-5, tile(7, 12)),
        position(red-circle-6, tile(5, 13))
    ])),
    assert(board(intermediate, 
    [
        position(cian-circle-1, tile(6, 7)),
        position(cian-square-1, tile(7, 9)),
        position(cian-triangle-1, tile(7,4)),
        position(cian-circle-3, tile(7, 3)),
        position(cian-pentagon-1, tile(5, 2)),
        position(cian-triangle-3, tile(2, 7)),
        position(cian-circle-6, tile(7, 7)),
        position(red-circle-1, tile(5, 4)),
        position(red-square-1, tile(7, 8)),
        position(red-pentagon-1, tile(5, 7)),
        position(red-circle-4, tile(4, 5)),
        position(red-triangle-1, tile(5, 13)),
        position(red-circle-6, tile(3, 7))
    ])),
    assert(board(final, 
    [
        position(cian-circle-1, tile(7, 3)),
        position(cian-pentagon-1, tile(7, 8)),
        position(red-circle-1, tile(5, 7))
    ])).
