:- ensure_loaded('state.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('board.pl').
:- ensure_loaded('logic.pl').

% menu/0
% Displays the menu and processes the user input, 1 to start the game, 2 to change settings, 3 to exit
menu :-
    display_menu,
    get_option(1, 3, 'Select', Option),
    processMenuOption(Option).

% processMenuOption(+Option)
% Processes the user input
processMenuOption(1) :-
    board(initial, Board),
    game_loop([Board, cian]),
    !.

processMenuOption(2) :-
    change_settings,
    menu.

processMenuOption(3) :-
    clear_screen,
    !.

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

% process_turn(+GameState, -NewGameState)
% Processes the turn of the current player
process_turn([Board, Player], [NewBoard, NewPlayer]) :-
    difficulty(Player, 1),
    % valid_moves([Board, Player], Moves),
    !,
    repeat,
    ask_move([Board, Player], OX-OY-DX-DY),
    % validate_move([Board, Player], OX-OY-DX-DY),
    move([Board, Player], OX-OY-DX-DY, [NewBoard, NewPlayer]),
    !.
% play/0
% Starts the game
play :-
    menu.

% valid_moves(+GameState, +Player, -Moves)
% Gets all the valid moves for the current player
valid_moves([Board, Player], Player, Moves) :-
    findall(OX-OY-DX-DY, validate_move([Board, Player], OX-OY-DX-DY), Moves).

% choose_move(+GameState, +Player, +Level, -Move).
% Chooses a move for the bot random player
choose_move([Board, Player], Player, 1, Move) :-
    valid_moves([Board, Player], Player, Moves),
    random_member(Move, Moves).