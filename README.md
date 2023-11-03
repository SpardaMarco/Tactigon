# Functional and Logic Programming - 1st Pratical Assignment
## Game Theme
**[Tactigon Board Game](https://tactigongame.com/)**
## Group Description
Group Name: **Tactigon_4**

Group Members:
- **[Marco André Pereira da Costa](https://github.com/SpardaMarco)** - up202108821 - 50% contribution
- **[Tiago Filipe Castro Viana](https://github.com/tiagofcviana)** - up201807126 - 50% contribution

## Installation and Execution
In order to install and execute the game, you must download ***PFL_TP1_T03_Tactigon_4.ZIP*** and extract it. Then, inside the *src* directory, consult the ***main.pl*** file through SICStus Prolog 4.8. Finally, run the following command:
```prolog
?- play.
```
To improve the game experience, we recommend maximizing the SICStus Prolog window and changing the font as follows:
- Font: Consolas
- Style: Normal
- Size: 11

The game is available for Windows and Linux.

## Game Description

**Tactigon** is a board game for two players, played on an irregular hexagon board. Each player starts with 13 pieces. The game starts with a default board configuration, and the players take turns moving their pieces and resolving any combat that may result from that movement.

General movement rules:
- Pieces can move along any path and in any direction up to their maximum spaces.
- Pieces can't jump over other pieces.*
- Maximum spaces are equal to the number of sides of the piece.**

\* - This rule can be changed by applying the advanced rule 1.<br>
** - This rule can be changed by applying the advanced rule 2.

Pieces can be one of this four types:
- **Circle** - 1 side
- **Triangle** - 3 sides
- **Square** - 4 sides
- **Pentagon** - 5 sides


![CombatTable](img/combatTable.png)
Figure 1 - Combat Table

The pieces can combat the opponent's pieces by moving to a tile occupied by an opposing piece.

Combat has two outcomes:
- The defending piece is captured, marked by a sword icon in Figure 1.
- Both pieces are captured, marked by a two swords crossing icon in Figure 1.

The sword and shield icon represents certain combats that can't occur in the game.<br>
Captured pieces are removed from the board and can't be used for the rest of the game.

The victory can be achieved by **capturing the opponent's pentagon** or by occupying **both gold tiles** at the **end** of the **opponent's turn**.

Two optional advanced rules can be applied to the game:
1. *Square pieces can jump over other pieces, except for opposing squares. A "jumped" tile still counts towards the piece's move limit.*
2. *Pieces that start a turn on a gold tile can move an additional space on that turn.*

For more information about the game, please consult the [official website](https://tactigongame.com/).<br>
For more information about the game rules, please consult the [How to Play](https://tactigongame.com/how-to-play/) or the [Rulebook](https://online.fliphtml5.com/hvuax/bvzo/).

## Game Logic

### Internal Game State Representation

**Board -** The board is represented by a list of Positions. Each Position is represented by 2 elements: a Piece and the Tile where the Piece is located. Each Tile consists of the coordinates (X, Y) on the board. The minimum and maximum values for the X coordinate is defined for each line, and there can only be tiles inside those limits. Finally, the board also has Gold Tiles, which are represented by the corresponding (X, Y) coordinates on the board with the predicate gold_tile.

**Player -** The game has only two players, cian and red, represented by the corresponding atoms. The first player to move is chosen randomly, and after each turn, the current player is changed to the other player using the other_play predicate.

The **GameState** is represented by a list with the **Board** and the **Player** at a given time in the game. The **GameState** does not contain a list of pieces that each player has captured, since they are removed from the board and can't be used for the rest of the game.

This is the representation of the board in the initial game state, where each piece is located in its starting position:
```prolog
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
```
*board.pl*

This is a possible representation of the board in an intermediate game state. The pieces are located in different positions than the initial game state, some of the pieces were captured, but no player has won yet, since both players still have their pentagon and the gold tiles ((1,5) and (5,1)) are not both occupied by the same player:

```prolog
% board(+State, -Board)
% Unifies Board with the board at the current State for starting a game or for demonstrating different board states
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
```
*board.pl*

And finally, a possible representation of the board in the final game state, where the cian player has won the game by capturing the red player's pentagon:
```prolog
% board(+State, -Board)
% Unifies Board with the board at the current State for starting a game or for demonstrating different board states
board(final, 
[
    position(cian-circle-1, tile(5, 1)),
    position(cian-pentagon-1, tile(5, 6)),
    position(red-circle-1, tile(3, 5))
]
).
```
*board.pl*

### Game State Visualization

### Move Validation and Execution

The game runs in a loop where each iteration corresponds to a turn. The only stop condition of this loop is the victory of one of the players:
```prolog
% game_loop(+GameState)
% Main game loop
game_loop(GameState) :-
    game_over(GameState, Winner),
    !,
    display_game(GameState),
    display_winner(Winner),
    !,
    menu.

game_loop(GameState) :-
    display_game(GameState),
    process_turn(GameState, NewGameState),
    !,
    game_loop(NewGameState).
```
*main.pl*

The process_turn predicate is responsible for processing the turn of the current player. If the current player is human, this predicate will ask for a move, validate it and make the move if it is valid. If the current player is a bot, this predicate will choose a valid move (depending on the difficulty level) and make the move:
```prolog
% process_turn(+GameState, -NewGameState)
% Processes the turn of the current player
process_turn([Board, Player], [NewBoard, NewPlayer]) :-
    difficulty(Player, 0),
    !,
    repeat,
    ask_move([Board, Player], OX-OY-DX-DY),
    validate_move([Board, Player], OX-OY-DX-DY),
    move([Board, Player], OX-OY-DX-DY, [NewBoard, NewPlayer]),
    !.

process_turn([Board, Player], [NewBoard, NewPlayer]) :-
    difficulty(Player, Difficulty),
    !,
    choose_move([Board, Player], Player, Difficulty, OX-OY-DX-DY),
    move([Board, Player], OX-OY-DX-DY, [NewBoard, NewPlayer]),
    !.

```
*main.pl*

The ask_move predicate is responsible for asking the user for a move and reading the input.

The validate_move predicate is responsible for validating the move. It starts by checking if the chosen piece is, in fact, on the board and if it belongs to the current player:
```prolog
% validate_move(+GameState, +Move)
% Checks if a move is valid
validate_move([Board, Player], OX-OY-DX-DY) :-
    member(position(Piece, tile(OX, OY)), Board),
    piece_info(Piece, Player, _),
    valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY).
``` 
*logic.pl*

After that, the predicate valid_move_for_piece is called to check if the move is valid for the chosen piece. This predicate starts by checking the type of the chosen piece, in order to determine the maximum number of spaces that the piece can move:
```prolog
% valid_move_for_piece(+GameState, +Piece, +Move)
% Checks if a move is valid for a particular piece
valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY) :-
    piece_info(Piece, _, Type), % Get the type of the piece
    movement(Type, N),  % N is the maximum number of steps for this type of piece
    valid_move_dfs([Board, Player], N, Piece, DX-DY, OX-OY).
```
*logic.pl*

If the advanced rule 2 is applied, the predicate valid_move_for_piece will also check if the chosen piece is on a gold tile, in order to determine if the piece can move an additional space:
```prolog
valid_move_for_piece([Board, Player], Piece, OX-OY-DX-DY) :-
    rules(2),
    gold_tile(OX, OY),
    piece_info(Piece, _, Type),
    movement(Type, N),  % N is the maximum number of steps for this type of piece
    N1 is N + 1, % N1 is the maximum number of steps increases by 1 because of additional rule 2 when piece is on a gold tile
    valid_move_dfs([Board, Player], N1, Piece, OX-OY-DX-DY, OX-OY).
```
*logic.pl*

After determining the maximum number of spaces that the piece can move, the predicate valid_move_dfs is called to check if the move is valid for the chosen piece. This predicate uses a depth-first search algorithm to check if the move is valid. The depth-first search algorithm starts by checking if the chosen piece can move to the destination tile in a single step. If it can, there are two possible outcomes: the destination tile is empty or the destination tile is occupied by an opposing piece. If the destination tile is empty, the move is valid. If the destination tile is occupied by an opposing piece, the move is valid if the combat is possible (i.e., the current player's piece can capture the opposing piece or the combat ends in a draw where both pieces are captured).

```prolog
% valid_move_dfs(+GameState, +N, +Piece, +DestinationTile, +CurrentTile)
% Checks if a move is valid for a particular piece using DFS, being N the number of steps left to take
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
```
*logic.pl*

If the chosen piece can't move to the destination tile in a single step, the depth-first search algorithm will move to an adjacent tile of the current piece and call itself recursively, decreasing the number of steps left to take by 1. This process will continue until the maximum number of spaces that the piece can move is reached (when N is equal to 0) and all possible moves are checked, in depth. Whitin the recursive calls, if there are still moves left to take, the algorithm will check if the destination tile is adjacent to the current tile and if it is empty or occupied by an opposing piece. If the destination tile is empty, the move is valid. If the destination tile is occupied by an opposing piece and the combat is possible the move is valid. If there are no more moves left to take, the move is invalid. As, by default, pieces can't jump other pieces, the algorithm will only make the recursive call if the chosen adjacent tile is empty:
```prolog
valid_move_dfs([Board, Player], N, Piece, DX-DY, CX-CY) :-
    N > 0,
    N1 is N - 1,
    findall(X-Y, adjacent(tile(CX, CY), tile(X, Y)), AdjacentTiles),
    member(CX1-CY1, AdjacentTiles), 
    \+ member(position(_, tile(CX1, CY1)), Board),
    valid_move_dfs([Board, Player], N1, Piece, DX-DY, CX1-CY1).
```
*logic.pl*

If the advanced rule 1 is applied, the predicate valid_move_dfs will also check if the chosen piece is a square, in order to determine if the piece can jump over other pieces (except for other squares):
```prolog
% Special case for the square piece when additional rule 1 is enabled.
% Square pieces can jump over other pieces, except for opposing squares. 
% A "jumped" tile still counts towards the piece's move limit
valid_move_dfs([Board, Player], N, Player-square-Id, DX-DY, CX-CY) :-
    rules(1),
    N > 0,
    N1 is N - 1,
    other_player(Player, Opponent),
    findall(X-Y, adjacent(tile(CX, CY), tile(X, Y)), AdjacentTiles), 
    member(CX1-CY1, AdjacentTiles), 
    \+ member(position(Opponent-square-_, tile(CX1, CY1)), Board),
    valid_move_dfs([Board, Player], N1, Player-square-Id, DX-DY, CX1-CY1).
```
*logic.pl*

### List of Valid Moves

### End of Game

### Game State Evaluation

### Computer Plays