:- ensure_loaded('state.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('board.pl').

% settings(+Player, -Difficulty)
% Gets the difficulty of the player. -1 for human player, 0 for easy, 1 for hard
:- dynamic settings/2.
settings(cian, -1).
settings(red, 0).

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
    play.

processMenuOption(3) :-
    clear_screen,
    !.

change_settings :-
    display_settings,
    get_option(1, 3, 'Select', Option),
    processSettingsOption(Option).
    
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

% play/0
% Starts the game
play :-
    menu.