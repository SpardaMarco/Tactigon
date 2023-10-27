:- ensure_loaded('state.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('board.pl').

% settings(+Player, -Difficulty)
% Gets the difficulty of the player. 1 for human player, 2 for easy, 3 for hard
:- dynamic settings/2.
settings(cian, 1).
settings(red, 2).

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

% change_settings/0
% Asks the user for new settings
change_settings :-
    ask_difficulty(cian),
    get_option(1, 3, 'Select an option', CianDifficulty),
    process_settings_option(cian, CianDifficulty),
    ask_difficulty(red),
    get_option(1, 3, 'Select an option', RedDifficulty),
    process_settings_option(red, RedDifficulty).

% process_settings_option(+Player, +Difficulty)
% Processes the user input and changes the settings
process_settings_option(P, NewDifficulty) :-
    retract(settings(P, _)),
    assert(settings(P, NewDifficulty)).
    
    
%temporary for testing
game_over(_, _) :-
    fail.

% game_loop(+GameState)
% Main game loop
game_loop(GameState) :-
    game_over(GameState, Winner),
    !,
    display_game(GameState),
    display_winner(Winner).

game_loop(GameState) :-
    display_game(GameState),
    process_turn(GameState, NewGameState),
    !,
    game_loop(NewGameState).

% process_turn(+GameState, -NewGameState)
% Processes the turn of the current player
process_turn([Board, Player], [NewBoard, NewPlayer]) :-
    settings(Player, 1),
    % valid_moves([Board, Player], Moves),
    !,
    repeat,
    ask_move([Board, Player], OX-OY-DX-DY),
    format('OX: ~d, OY: ~d, DX: ~d, DY: ~d~n', [OX, OY, DX, DY]),

    % move([Board, Player], OX-OY-DX-DY, [NewBoard, NewPlayer]),
    !.
% play/0
% Starts the game
play :-
    menu.