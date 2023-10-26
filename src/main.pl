:- ensure_loaded('state.pl').

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
    initial(GameState),
    game_loop(GameState).