:- ensure_loaded('utils.pl').
    
% board(+State, -Board)
% Unifies Board with the board at the current State

board(initial, 
[
    position(cianCircle1, tile(3, 0)),
    position(cianCircle2, tile(1, 1)),
    position(cianSquare1, tile(2, 1)),
    position(cianTriangle1, tile(3, 1)),
    position(cianSquare2, tile(4, 1)),
    position(cianCircle3, tile(5, 1)),
    position(cianTriangle2, tile(2, 2)),
    position(cianPentagon, tile(3, 2)),
    position(cianTriangle3, tile(4, 2)),
    position(cianCircle4, tile(1, 3)),
    position(cianSquare3, tile(3, 3)),
    position(cianCircle5, tile(5, 3)),
    position(cianCircle6, tile(3, 4)),
    position(redCircle1, tile(3, 6)),
    position(redCircle2, tile(1, 7)),
    position(redTriangle1, tile(2, 7)),
    position(redSquare1, tile(3, 7)),
    position(redTriangle2, tile(4, 7)),
    position(redCircle3, tile(5, 7)),
    position(redSquare2, tile(2, 8)),
    position(redPentagon, tile(3, 8)),
    position(redSquare3, tile(4, 8)),
    position(redCircle4, tile(1, 9)),
    position(redTriangle3, tile(3, 9)),
    position(redCircle5, tile(5, 9)),
    position(redCircle6, tile(3, 10))
]
).


% tile(+X, +Y)
% This predicate is true if there is a tile at coordinates X, Y on the board.
tile(X, Y) :- 
    line(Y, MinX, MaxX),
    between(MinX, MaxX, X),
    !.

% goldTile(+X, +Y)
% Gold tiles on the board
goldTile(1, 5).
goldTile(5, 5).

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
% Unifies Movement with the movement of Piece
movement(circle, 1).
movement(triangle, 3).
movement(square, 4).
movement(pentagon, 5).

% player(+Player)
% All players
player(cian).
player(red).

% opponent(+Player, -Opponent)
% Unifies Opponent with the opponent of Player
opponent(cian, red).
opponent(red, cian).

% piece_info(+Piece, -Player, -Type)
% Unifies Player and Type with the player and type of Piece
piece_info(cianCircle1, cian, circle).
piece_info(cianCircle2, cian, circle).
piece_info(cianCircle3, cian, circle).
piece_info(cianCircle4, cian, circle).
piece_info(cianCircle5, cian, circle).
piece_info(cianCircle6, cian, circle).
piece_info(cianTriangle1, cian, triangle).
piece_info(cianTriangle2, cian, triangle).
piece_info(cianTriangle3, cian, triangle).
piece_info(cianSquare1, cian, square).
piece_info(cianSquare2, cian, square).
piece_info(cianSquare3, cian, square).
piece_info(cianPentagon, cian, pentagon).
piece_info(redCircle1, red, circle).
piece_info(redCircle2, red, circle).
piece_info(redCircle3, red, circle).
piece_info(redCircle4, red, circle).
piece_info(redCircle5, red, circle).
piece_info(redCircle6, red, circle).
piece_info(redTriangle1, red, triangle).
piece_info(redTriangle2, red, triangle).
piece_info(redTriangle3, red, triangle).
piece_info(redSquare1, red, square).
piece_info(redSquare2, red, square).
piece_info(redSquare3, red, square).
piece_info(redPentagon, red, pentagon).

% piece(+Piece)
% All pieces
piece(Piece) :- piece_info(Piece, _, _).

% adjacent(tile(+X, +Y), tile(+X1, +Y1))
% Rules for adjacent tiles
adjacent(tile(X, Y), tile(X1, Y)) :- 
    tile(X, Y),
    tile(X1, Y),
    DIFX is X - X1,
    abs(DIFX, ABSX),
    ABSX == 1,
    !.
adjacent(tile(X, Y), tile(X, Y1)) :- 
    tile(X, Y),
    tile(X, Y1),
    DIFY is Y - Y1,
    abs(DIFY, ABSY),
    ABSY == 1,
    !.
adjacent(tile(X, Y), tile(X1, Y1)) :-
    1 is X mod 2,
    DIFY is Y - Y1,
    DIFY == 1,
    DIFX is X1 - X,
    abs(DIFX, ABSX),
    ABSX == 1,
    !.
adjacent(tile(X, Y), tile(X1, Y1)) :-
    0 is X mod 2,
    DIFY is Y - Y1,
    DIFY == -1,
    DIFX is X1 - X,
    abs(DIFX, ABSX),
    ABSX == 1,
    !.

draw_header :-
    write('   |X |X0|X1|X2|X3|X4|X5|X6|  |'), nl,
    write('---|--------------------------|---'), nl.

draw_footer :-
    write('---|--------------------------|---'), nl,
    write('   |X |X0|X1|X2|X3|X4|X5|X6|  |'), nl.

draw_board(Board) :-
    write('Y  |            __            |Y'), nl,
    draw_board_lines(Board, 0, 10).

draw_board_lines(_, Y, MaxY) :- 
    Y > MaxY,
    !.

draw_board_lines(Board, Y, MaxY) :-
    draw_board_line(Board, Y),
    Y1 is Y + 1,
    !,
    draw_board_lines(Board, Y1, MaxY).

draw_board_line(Board, Y) :-
    Y < 10,
    format('Y~d |', [Y]),
    print_n(26, ' '),
    format('|Y~d', [Y]),
    nl,
    format('Y~d |', [Y]),
    print_n(26, ' '),
    format('|Y~d', [Y]),
    nl.

draw_board_line(Board, Y) :-
    format('Y~d|', [Y]),
    print_n(26, ' '),
    format('|Y~d', [Y]),
    nl,
    format('Y~d|', [Y]),
    print_n(26, ' '),
    format('|Y~d', [Y]),
    nl.

    

draw_temp(Board) :-
    draw_header,
    draw_board(Board),
    draw_footer.