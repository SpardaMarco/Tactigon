:- use_module(library(lists)).
:- ensure_loaded('board.pl').

% Movement Logic

% validate_move(+GameState, +Move)
% Checks if a move is valid
validate_move([Board, Player], OX-OY-DX-DY) :-
    member(position(Piece, tile(OX, OY)), Board),
    piece_info(Piece, Player, _), % Check if the piece belongs to the player
    valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY).

% valid_move_for_piece(+GameState, +Piece, +Move)
% Checks if a move is valid for a particular piece
valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY) :-
    piece_info(Piece, _, Type), % Get the type of the piece
    movement(Type, N),  % N is the maximum number of steps for this type of piece
    valid_move_dfs([Board, Player], N, Piece, OX-OY-DX-DY, OX-OY).

% valid_move_dfs(+GameState, +N, +Piece, +Move, +CurrentTile)
% Checks if a move is valid for a particular piece using DFS
valid_move_dfs([Board, Player], N, _, OX-OY-DX-DY, CX-CY) :-
    N > 0,
    adjacent(tile(CX, CY), tile(DX, DY)), % Check if the destination tile is adjacent to the current tile
    \+ member(position(_, tile(DX, DY)), Board),  % Check if the tile is empty
    !.

valid_move_dfs([Board, Player], N, Piece, OX-OY-DX-DY, CX-CY) :-
    N > 0,
    adjacent(tile(CX, CY), tile(DX, DY)), % Check if the destination tile is adjacent to the current tile
    member(position(Defender, tile(DX, DY)), Board),  % Check if the destination tile is occupied
    other_player(Player, DefenderPlayer), 
    piece_info(Defender, DefenderPlayer, DefenderType),  % Check if the destination tile is occupied by an enemy piece
    piece_info(Piece, Player, Type), 
    combat(Type, DefenderType, _), % Check if the piece can attack the enemy piece
    !.

valid_move_dfs([Board, Player], N, Piece, OX-OY-DX-DY, CX-CY) :-
    N > 0,
    N1 is N - 1,
    findall(X-Y, adjacent(tile(CX, CY), tile(X, Y)), AdjacentTiles), % Get all the adjacent tiles
    member(CX1-CY1, AdjacentTiles), % Get an adjacent tile
    \+ member(position(_, tile(CX1, CY1)), Board),  % Check if the tile is empty  
    valid_move_dfs([Board, Player], N1, Piece, OX-OY-DX-DY, CX1-CY1).


% move(+GameState, +Move, -NewGameState)
% Moves a piece from one tile to another
move([Board, Player], OX-OY-DX-DY, [NewBoard, NewPlayer]) :-
    member(position(Piece, tile(OX, OY)), Board),
    member(position(Defender, tile(DX, DY)), Board),
    piece_info(Piece, _, Type),
    piece_info(Defender, _, DefenderType),
    combat(Type, DefenderType, none),
    !,
    delete(Board, position(Piece, tile(OX, OY)), Board1),
    delete(Board1, position(Defender, tile(DX, DY)), NewBoard),
    other_player(Player, NewPlayer).

move([Board, Player], OX-OY-DX-DY, [NewBoard, NewPlayer]) :-
    member(position(Piece, tile(OX, OY)), Board),
    !,
    delete(Board, position(Piece, tile(OX, OY)), Board1),
    delete(Board1, position(Defender, tile(DX, DY)), Board2),
    append(Board2, [position(Piece, tile(DX, DY))], NewBoard),
    other_player(Player, NewPlayer).

% valid_moves(+GameState, +Player, -Moves)
% Gets all the valid moves for the current player
valid_moves([Board, Player], Player, Moves) :-
    setof(OX-OY-DX-DY, validate_move([Board, Player], OX-OY-DX-DY), Moves).

% choose_move(+GameState, +Player, +Level, -Move).
% Chooses a move for the difficulty level 1 (random) bot
choose_move([Board, Player], Player, 1, Move) :-
    valid_moves([Board, Player], Player, Moves),
    random_member(Move, Moves).


% Game Over Logic

% game_over(+GameState, -Winner)
% Checks if the game is over and returns the winner
game_over([Board, Player], Winner) :-
    \+ member(position(Player-pentagon-_, tile(_, _)), Board),
    other_player(Player, Winner).

game_over([Board, Player], Player) :-
    findall(X-Y, gold_tile(X, Y), GoldTiles),
    findall(X-Y, (member(position(Player-_-_, tile(X, Y)), Board), member(X-Y, GoldTiles)), GoldTilesWithPlayer),
    length(GoldTiles, N),
    length(GoldTilesWithPlayer, N).


% Combat Logic

% combat(+Attacker, +Defender, -Winner)
% Returns the winner of a combat between an Attacker and  a Defender
combat(circle, _, circle).
combat(triangle, circle, none) :- !.
combat(triangle, _, triangle).
combat(square, triangle, none) :- !.
combat(square, Defender, Winner) :- 
    Defender \= circle,
    Winner = square.
combat(pentagon, pentagon, pentagon).