:- ensure_loaded('utils.pl').
    
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

% draw_header\0
% Draws the header of the board
draw_header :-
    write('   |X |X0|X1|X2|X3|X4|X5|X6|  |'), nl,
    write('---|--------------------------|---'), nl,
    write('Y  |            __            |Y'), nl.

% draw_footer\0
% Draws the footer of the board
draw_footer :-
    write('---|--------------------------|---'), nl,
    write('   |X |X0|X1|X2|X3|X4|X5|X6|  |'), nl.

% draw_board(+Board)
% Draws the board
draw_board(Board) :-
    MaxY is 2*10 + 1,
    draw_header,
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
build_line(_, _, _, 7, Aux, Line) :-
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
    (tile(X, PY) -> append(Aux, [draw(bottom, _)], Aux1);
    append(Aux, [draw(startBottom, _)], Aux1)),
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