:- ensure_loaded('state.pl').
:- ensure_loaded('interface.pl').

menu([Board, Player]) :-
    display_menu,
    get_option(1, 3, 'Select', Option),
    processMenuOption(Option, [Board, Player]).

processMenuOption(1, [Board, Player]) :-
    !.

processMenuOption(2, _) :-
    change_settings,
    play.

processMenuOption(3, _). 


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
    menu(GameState).