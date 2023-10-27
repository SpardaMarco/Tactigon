:- ensure_loaded('utils.pl').

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