:- ensure_loaded('utils.pl').
:- ensure_loaded('board.pl').
:- ensure_loaded('logic.pl').
:- ensure_loaded('settings.pl').

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
display_game([Board, _]) :-
    clear_screen,
    nl,
    draw_board(Board),
    display_legend,
    nl,nl.

% display_legend/0
% Displays the legend of the game
display_legend :-
    write('Legend:'), nl,
    write('CC - Cian Circle'), write('    '), write('RC - Red Circle'), nl,
    write('CT - Cian Triangle'), write('  '), write('RT - Red Triangle'), nl,
    write('CS - Cian Square'), write('    '), write('RS - Red Square'), nl,
    write('CP - Cian Pentagon'), write('  '), write('RP - Red Pentagon'), nl,
    display_gold_tiles.

% display_gold_tiles/0
% Displays the gold tiles positions in the board
display_gold_tiles :-
    findall(X-Y, gold_tile(X, Y), GoldTiles),
    write('Gold Tiles (GT):'), write(' '), 
    display_gold_tiles(GoldTiles),
    nl.

% display_gold_tiles(+GoldTiles)
% Displays the gold tiles positions in the board
display_gold_tiles([]) :-
    nl.

% If there is only 1 tile, there is no need to display the comma
display_gold_tiles([X-Y]) :-
    format('(~d, ~d)', [X, Y]),
    !.

display_gold_tiles([X-Y|T]) :-
    format('(~d, ~d)', [X, Y]),
    write(', '),
    display_gold_tiles(T).    

% display_winner(+Winner)
% Displays the winner of the game
display_winner(Winner) :-
    write('The winner is: '), write(Winner), nl,
    write('Press any key to return to the menu.'), nl,
    get_char(_).

% ask_difficulty(+Player)
% Displays the difficulty options for the player
ask_difficulty(P) :-
    write('Player '), write(P), write(' is:'), nl,
    write('1 - Level 1 Bot (Random)'), nl,
    write('2 - Level 2 Bot (Greedy)'), nl,
    write('3 - Human'), nl.

% ask_rules/0
% Displays the advanced rules options
ask_rules :-
    write('Advanced Rules:'), nl,
    write('1 - Square pieces can jump over other pieces, except for opposing squares. A "jumped" tile still counts towards the piece\'s move limit.'), nl,
    write('2 - Pieces that start a turn on a gold tile can move an additional space on that turn.'), nl,
    write('Options:'), nl,
    write('1 - Advanced Rule 1'), nl,
    write('2 - Advanced Rule 2'), nl,
    write('3 - Both Advanced Rules'), nl,
    write('4 - None'), nl.

% invalid_move/0
% Displays an error message for an invalid move when it is the second time this predicate is called because of a failed move
invalid_move.
invalid_move :-
    write('Invalid move!'), nl,
    fail.

% get_move(+GameState, -Move)
% Gets a move from the user
% If OX-OY = DX-DY, then the move is cancelled, and the user is asked for a new move
get_move([Board, Player], OX-OY-DX-DY) :-
    ask_move([Board, Player], AOX-AOY-ADX-ADY),
    check_cancel_move([Board, Player], AOX-AOY-ADX-ADY, OX-OY-DX-DY),
    !.

% check_cancel_move(+GameState, +Move, -NewMove)
% Checks if the move is cancelled
check_cancel_move([Board, Player], AOX-AOY-AOX-AOY, Move) :-
    write('Move cancelled!'), nl,
    !,
    get_move([Board, Player], Move).

check_cancel_move(_, Move, Move) :-
    !.

% ask_move(+GameState, -Move)
% Asks the player for a move
ask_move([_, Player], OX-OY-DX-DY) :-
    ask_move_input(Player, 'Player ~w, please choose a piece to move (X-Y): ', OX-OY),
    format('To cancel the move, please enter ~d-~d.', [OX, OY]), nl,
    ask_move_input(Player, 'Player ~w, please where to move the piece (X-Y): ', DX-DY),
    !.

% ask_move_input(+Player, +Context, -X-Y)
% Asks the player for a move
ask_move_input(Player, Context, X-Y) :-
    format(Context, [Player]),
    get_move_input(X-Y),
    !.

ask_move_input(Player, Context, X-Y) :-
    write('Invalid input!'), nl,
    ask_move_input(Player, Context, X-Y).

% ask_board_size/0
% Asks the user for the board size
ask_board_size :-
    write('Board Size:'), nl,
    write('1 - 11 lines, 7 columns (Default board size)'), nl,
    write('2 - 13 lines, 9 columns'), nl,
    write('3 - 15 lines, 11 columns'), nl.

% change_settings/0
% Asks the user for new settings
change_settings :-
    ask_board_size,
    get_option(1, 3, 'Select an option', 'option', BoardSize),
    process_board_size_option(BoardSize),
    ask_difficulty(cian),
    get_option(1, 3, 'Select an option', 'option', CianDifficulty),
    process_difficulty_option(cian, CianDifficulty),
    ask_difficulty(red),
    get_option(1, 3, 'Select an option', 'option', RedDifficulty),
    process_difficulty_option(red, RedDifficulty),
    ask_rules,
    get_option(1, 4, 'Select an option', 'option', Rules),
    process_rules_option(Rules).

% process_difficulty_option(+Player, +NewDifficulty)
% Processes the user input and changes the settings regarding the difficulty of each player
process_difficulty_option(P, NewDifficulty) :-
    retract(difficulty(P, _)),
    assert(difficulty(P, NewDifficulty)).

% process_rules_option(+NewRules)
% Processes the user input and changes the settings regarding the advanced rules
process_rules_option(NewRules) :-
    NewRules =:= 3,
    retract(rules(_)),
    assert(rules(1)),
    assert(rules(2)),
    !.

process_rules_option(NewRules) :-
    retract(rules(_)),
    assert(rules(NewRules)).

% process_board_size_option(+NewBoardSize)
% Processes the user input and changes the settings regarding the board size
process_board_size_option(1) :-
    create_new_board(11).

process_board_size_option(2) :-
    create_new_board(13).

process_board_size_option(3) :-
    create_new_board(15).