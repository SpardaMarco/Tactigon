:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('board.pl').
:- ensure_loaded('settings.pl').

% Movement Logic

% validate_move(+GameState, +Move)
% Checks if a move is valid
validate_move([Board, Player], OX-OY-DX-DY) :-
    member(position(Piece, tile(OX, OY)), Board),
    piece_info(Piece, Player, _),
    valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY).

% valid_move_for_piece(+GameState, +Piece, +Move)
% Checks if a move is valid for a particular piece
valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY) :-
    rules(2),
    gold_tile(OX, OY),
    piece_info(Piece, _, Type),
    movement(Type, N),  % N is the maximum number of steps for this type of piece
    N1 is N + 1, % N1 is the maximum number of steps increases by 1 because of additional rule 2 when piece is on a gold tile
    valid_move_dfs([Board, Player], N1, Piece, OX-OY-DX-DY, OX-OY).

valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY) :-
    piece_info(Piece, _, Type), % Get the type of the piece
    movement(Type, N),  % N is the maximum number of steps for this type of piece
    valid_move_dfs([Board, Player], N, Piece, DX-DY, OX-OY).

% valid_move_dfs(+GameState, +N, +Piece, +DestinationTile, +CurrentTile)
% Checks if a move is valid for a particular piece using DFS
valid_move_dfs([Board, _], N, _, DX-DY, CX-CY) :-
    N > 0,
    adjacent(tile(CX, CY), tile(DX, DY)),
    \+ member(position(_, tile(DX, DY)), Board). 

valid_move_dfs([Board, Player], N, Piece, DX-DY, CX-CY) :-
    N > 0,
    adjacent(tile(CX, CY), tile(DX, DY)),
    member(position(Defender, tile(DX, DY)), Board),  % Check if the destination tile is occupied
    other_player(Player, DefenderPlayer), 
    piece_info(Defender, DefenderPlayer, DefenderType),  % Check if the destination tile is occupied by an opponent's piece
    piece_info(Piece, Player, Type), 
    combat(Type, DefenderType, _). % Check if the piece can attack the opponent's piece.

% Special case for the square piece when additional rule 1 is enabled
valid_move_dfs([Board, Player], N, Player-square-Id, DX-DY, CX-CY) :-
    rules(1),
    N > 0,
    N1 is N - 1,
    other_player(Player, Opponent),
    findall(X-Y, adjacent(tile(CX, CY), tile(X, Y)), AdjacentTiles), 
    member(CX1-CY1, AdjacentTiles), 
    \+ member(position(Opponent-square-_, tile(CX1, CY1)), Board),
    valid_move_dfs([Board, Player], N1, Player-square-Id, DX-DY, CX1-CY1).

valid_move_dfs([Board, Player], N, Piece, DX-DY, CX-CY) :-
    N > 0,
    N1 is N - 1,
    findall(X-Y, adjacent(tile(CX, CY), tile(X, Y)), AdjacentTiles),
    member(CX1-CY1, AdjacentTiles), 
    \+ member(position(_, tile(CX1, CY1)), Board),
    valid_move_dfs([Board, Player], N1, Piece, DX-DY, CX1-CY1).

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
    delete(Board1, position(_, tile(DX, DY)), Board2),
    append(Board2, [position(Piece, tile(DX, DY))], NewBoard),
    other_player(Player, NewPlayer).

% valid_moves(+GameState, +Player, -Moves)
% Gets all the valid moves for the current player
valid_moves([Board, Player], Player, Moves) :-
    setof(OX-OY-DX-DY, [Board, Player]^validate_move([Board, Player], OX-OY-DX-DY), Moves).

valid_moves([_, Player], Opponent, Moves) :-
    Opponent \= Player,
    Moves = [].

% choose_move(+GameState, +Player, +Level, -Move).
% Chooses a move for the difficulty level 1 (random) bot
choose_move([Board, Player], Player, 1, Move) :-
    valid_moves([Board, Player], Player, Moves), % Get all the valid moves
    random_member(Move, Moves). % Choose a random move

% choose_move(+GameState, +Player, +Level, -Move).
% Chooses a move for the difficulty level 2 (greedy) bot
choose_move([Board, Player], Player, 2, Move) :-
    valid_moves([Board, Player], Player, Moves), % Get all valid moves for the player
    findall(Value-CurrentMove, (member(CurrentMove, Moves), move([Board, Player], CurrentMove, [NewBoard, NewPlayer]), value([NewBoard, NewPlayer], Player, Value)), ValuesMoves), % Get the value of the game state after each move
    sort(ValuesMoves, SortedValuesMoves), % Sort the list of values and moves
    reverse(SortedValuesMoves, ReversedValuesMoves), % Get the move with the highest value
    ReversedValuesMoves = [MaxValue-_|_],
    select_max_value_move(ReversedValuesMoves, MaxValue, Move), % Select a move with the highest value
    !.

% select_max_value_move(+ValuesMoves, +MaxValue, -Move)
% Selects a move with the given maximum value
select_max_value_move(ValuesMoves, MaxValue, Move) :- 
    findall(M, (member(MaxValue-M, ValuesMoves)), Moves), % Get all the moves with the given maximum value
    random_member(Move, Moves), % Choose a random move with the given maximum value
    !.

% value(+GameState, +EvaluatedPlayer, -Value)
% Evaluate the value of the game state for the given player
value([Board, _], EvaluatedPlayer, Value) :-
    evaluate_advantage([Board, _], EvaluatedPlayer, Advantage),
    closest_to_opponent_pentagon([Board, _], EvaluatedPlayer, Distance),
    Value is Advantage - Distance.

% evaluate_advantage(+GameState, +EvaluatedPlayer, -Advantage)
% Evaluate the advantage of the game state for the given player
evaluate_advantage([Board, _], EvaluatedPlayer, Advantage) :-
    count_player_pieces([Board, _], EvaluatedPlayer, NumPlayerPieces),
    other_player(EvaluatedPlayer, Opponent),    
    count_player_pieces([Board, _], Opponent, NumOpponentPieces),
    Advantage is NumPlayerPieces - NumOpponentPieces.

% count_player_pieces(+GameState, +EvaluatedPlayer, -NumPieces)
% Count the number of pieces for the given player in the game state
count_player_pieces([Board, _], EvaluatedPlayer, NumPieces) :-
    findall(_, (member(position(EvaluatedPlayer-_-_, _), Board)), PlayerPieces),
    length(PlayerPieces, NumPieces).

% closest_to_opponent_pentagon(+GameState, +EvaluatedPlayer, -Distance)
% Find the piece of the given player that is closest to the opponent's pentagon
closest_to_opponent_pentagon([Board, _], EvaluatedPlayer, MinDistance) :-
    findall(Position, (member(position(EvaluatedPlayer-_-_, Position), Board)), PlayerPiecesPositions), % Get all the pieces of the player
    other_player(EvaluatedPlayer, Opponent),
    member(position(Opponent-pentagon-_, OpponentPiecePosition), Board), 
    setof(Distance, Position^PlayerPiecesPositions^OpponentPiecePosition^(member(Position, PlayerPiecesPositions), distance(Position, OpponentPiecePosition, Distance)), [MinDistance|_]). % Get the distance between the player's piece and the opponent's pentagon


% Game Over Logic

% game_over(+GameState, -Winner)
% Checks if the game is over and returns the winner
game_over([Board, Player], Winner) :-
    \+ member(position(Player-pentagon-_, tile(_, _)), Board),
    other_player(Player, Winner).

game_over([Board, Player], Player) :-
    other_player(Player, Opponent),
    \+ member(position(Opponent-pentagon-_, tile(_, _)), Board).

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