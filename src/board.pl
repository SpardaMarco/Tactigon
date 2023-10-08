:- ensure_loaded('utils.pl').
    
% board(+State, -Board)
% Unifies Board with the board at the current State

board(initial, 
[
    position(cianCircle1, tile(1, 1)),
    position(cianCircle2, tile(3, 1)),
    position(redCircle1, tile(7, 1)),
    position(redCircle2, tile(9, 1)),
    position(cianSquare1, tile(1, 2)),
    position(cianTriangle1, tile(2, 2)),
    position(redTriangle1, tile(7, 2)),
    position(redSquare1, tile(8, 2)),
    position(cianCircle3, tile(0, 3)),
    position(cianTriangle2, tile(1, 3)),
    position(cianPentagon, tile(2, 3)),
    position(cianSquare2, tile(3, 3)),
    position(cianCircle4, tile(4, 3)),
    position(redCircle3, tile(6, 3)),
    position(redSquare2, tile(7, 3)),
    position(redPentagon, tile(8, 3)),
    position(redTriangle2, tile(9, 3)),
    position(redCircle4, tile(10, 3)),
    position(cianSquare3, tile(1, 4)),
    position(cianTriangle3, tile(2, 4)),
    position(redTriangle3, tile(7, 4)),
    position(redSquare3, tile(8, 4)),
    position(cianCircle5, tile(1, 5)),
    position(cianCircle6, tile(3, 5)),
    position(redCircle5, tile(7, 5)),
    position(redCircle6, tile(9, 5))
]
).


% tile(+X, +Y)
% Tiles on the board
tile(4, 0).
tile(5, 0).
tile(1, 1).
tile(2, 1).
tile(3, 1).
tile(4, 1).
tile(5, 1).
tile(6, 1).
tile(7, 1).
tile(8, 1).
tile(9, 1).
tile(0, 2).
tile(1, 2).
tile(2, 2).
tile(3, 2).
tile(4, 2).
tile(5, 2).
tile(6, 2).
tile(7, 2).
tile(8, 2).
tile(9, 2).
tile(1, 3).
tile(2, 3).
tile(3, 3).
tile(4, 3).
tile(5, 3).
tile(6, 3).
tile(7, 3).
tile(8, 3).
tile(9, 3).
tile(10, 3).
tile(0, 4).
tile(1, 4).
tile(2, 4).
tile(3, 4).
tile(4, 4).
tile(5, 4).
tile(6, 4).
tile(7, 4).
tile(8, 4).
tile(9, 4).
tile(1, 5).
tile(2, 5).
tile(3, 5).
tile(4, 5).
tile(5, 5).
tile(6, 5).
tile(7, 5).
tile(8, 5).
tile(9, 5).
tile(4, 6).
tile(5, 6).

% goldTile(+X, +Y)
% Gold tiles on the board
goldTile(5, 1).
goldTile(5, 5).

% line(+Y, -MinX, -MaxX)
% Unifies MinX and MaxX with the minimum and maximum X values of the line Y
line(0, 4, 5).
line(1, 1, 9).
line(2, 0, 9).
line(3, 0, 10).
line(4, 0, 9).
line(5, 1, 9).
line(6, 4, 5).

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
    1 is Y mod 2,
    DIFX is X - X1,
    DIFX == 1,
    DIFY is Y1 - Y,
    abs(DIFY, ABSY),
    ABSY == 1,
    !.
adjacent(tile(X, Y), tile(X1, Y1)) :-
    0 is Y mod 2,
    DIFX is X - X1,
    DIFX == -1,
    DIFY is Y1 - Y,
    abs(DIFY, ABSY),
    ABSY == 1,
    !.
    

