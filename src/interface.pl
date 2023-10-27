:- ensure_loaded('utils.pl').
:- ensure_loaded('board.pl').
:- ensure_loaded('logic.pl').

% draw_title/0
% Draws the title banner of the game
draw_title :-
    write('  _______         _   _                   '),nl,
    write(' |__   __|       | | (_)                  '),nl,
    write('    | | __ _  ___| |_ _  __ _  ___  _ __  '),nl,
    write('    | |/ _` |/ __| __| |/ _` |/ _ \\| \'_ \\ '),nl,
    write('    | | (_| | (__| |_| | (_| | (_) | | | |'),nl,
    write('    |_|\\__,_|\\___|\\__|_|\\__, |\\___/|_| |_|'),nl,
    write('                         __/ |            '),nl,
    write('                        |___/             '),nl.

% draw_menu_options/0
% Draws the menu options
draw_menu_options :-
    write('1 - Play'),nl,
    write('2 - Settings'),nl,
    write('3 - Exit'),nl.

% display_menu/0
% Displays the menu
display_menu :-
    clear_screen,
    draw_title,
    nl,nl,
    draw_menu_options,
    nl,nl.

% display_game(+GameState)
% Displays the game and all its elements
display_game(GameState) :-
    clear_screen,
    nl,
    draw_board(GameState),
    display_legend,
    nl,nl.

display_legend :-
    write('Legend:'), nl,
    write('CC - Cian Circle'), write('    '), write('RC - Red Circle'), nl,
    write('CT - Cian Triangle'), write('  '), write('RT - Red Triangle'), nl,
    write('CS - Cian Square'), write('    '), write('RS - Red Square'), nl,
    write('CP - Cian Pentagon'), write('  '), write('RP - Red Pentagon'), nl,
    write('Gold Tiles (GT):'), write(' '), write('(1, 5)'), write(', '), write('(5, 5)'), nl.

% ask_difficulty(+Player)
% Displays the difficulty options for the player
ask_difficulty(P) :-
    write('Player '), write(P), write(' is:'), nl,
    write('1 - Human'), nl,
    write('2 - Level 1 Bot (Random)'), nl,
    write('3 - Level 2 Bot (Greedy)'), nl.

ask_move([Board, Player], OX-OY-DX-DY) :-
    format('Player ~w, please choose a piece to move (X-Y): ', [Player]),
    get_move_input(Player, OX-OY),
    format('Player ~w, please where to move the piece (X-Y): ', [Player]),
    get_move_input(Player, DX-DY).

