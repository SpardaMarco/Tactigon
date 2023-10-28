:- ensure_loaded('board.pl').

validate_move([Board, Player], OX-OY-DX-DY) :-
    member(position(Piece, tile(OX, OY)), Board),
    piece_info(Piece, Player, Type),
    movement(Type, Movement).




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
combat(circle, _, circle).
combat(triangle, circle, none) :- !.
combat(triangle, _, triangle).
combat(square, triangle, none) :- !.
combat(square, Defender, Winner) :- 
    Defender \= circle,
    Winner = square.