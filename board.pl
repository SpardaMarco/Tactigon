
% board(+State, -Matrix)
% Initial board structure

% Maybe é preciso mudar.

board(initial, 
[
[noTile, noTile, noTile, noTile, blueTile, blueTile, noTile, noTile, noTile, noTile],
[noTile, cianCircle, tile, cianCircle, blueTile, goldTile, blueTile, redCircle, tile, redCircle, noTile],
[tile, cianSquare, cianTriangle, tile, blueTile, blueTile, tile, redTriangle, redSquare, tile],
[cianCircle, cianTriangle, cianPentagon, cianSquare, cianCircle, blueTile, redCircle, redSquare, redPentagon, redTriangle, redCircle],
[tile, cianSquare, cianTriangle, tile, blueTile, blueTile, tile, redTriangle, redSquare, tile],
[noTile, cianCircle, tile, cianCircle, blueTile, goldTile, blueTile, redCircle, tile, redCircle, noTile],
[noTile, noTile, noTile, noTile, blueTile, blueTile, noTile, noTile, noTile, noTile],
]
).

% piece(+Piece, ?Player, -Type)

piece(cianCircle, cian, circle).
piece(cianSquare, cian, square).
piece(cianTriangle, cian, triangle).
piece(cianPentagon, cian, pentagon).
piece(redCircle, red, circle).
piece(redSquare, red, square).
piece(redTriangle, red, triangle).
piece(redPentagon, red, pentagon).
piece(goldTile, neutral, goldTile).
piece(tile, neutral, tile).
piece(blueTile, neutral, tile).
piece(noTile, neutral, noTile).

