:- ensure_loaded('interface.pl').
:- ensure_loaded('board.pl').
:- ensure_loaded('logic.pl').

% menu/0
% Displays the menu and processes the user input. 1 to start the game, 2 to change settings and 3 to exit
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

% play/0
% Starts the game
play :-
    menu.