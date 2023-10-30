:- use_module(library(lists)).
:- ensure_loaded('board.pl').

% Movement Logic

% validate_move(+GameState, +Move)
% Checks if a move is valid
validate_move([Board, Player], OX-OY-DX-DY) :-
    member(position(Piece, tile(OX, OY)), Board),
    piece_info(Piece, Player, Type),
    valid_move_for_piece(Board, Player, Type, OX, OY, DX, DY).

% valid_move_for_piece(+Board, +Player, +Piece, +OX, +OY, +DX, +DY)
% Checks if a move is valid for a particular piece on the board
valid_move_for_piece(Board, Player, Piece, OX, OY, DX, DY) :-
    movement(Piece, N),  % N is the maximum number of steps for this particular piece
    valid_move_bfs(Board, Player, N, Piece, OX, OY, DX, DY).

% valid_move_bfs(+Board, +Player, +N, +Piece, +OX, +OY, +DX, +DY)
% Checks if the move is valid using BFS for N-1 levels
valid_move_bfs(_, _, 0, _, OX, OY, DX, DY) :-
    % If we have completed N-1 levels of BFS, check if DX-DY is adjacent to the current OX-OY
    adjacent(tile(OX, OY), tile(DX, DY)).

valid_move_bfs(Board, Player, N, Piece, OX, OY, DX, DY) :-
    N > 0,
    N1 is N - 1,
    adjacent(tile(OX, OY), tile(OX1, OY1)),  
    \+ member(position(_, tile(OX1, OY1)), Board),  % Check if the destination tile is empty
    valid_move_bfs(Board, Player, N1, Piece, OX1, OY1, DX, DY).

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
% Returns the winner of a combat between Attacker and Defender
combat(circle, _, circle).
combat(triangle, circle, none) :- !.
combat(triangle, _, triangle).
combat(square, triangle, none) :- !.
combat(square, Defender, Winner) :- 
    Defender \= circle,
    Winner = square.