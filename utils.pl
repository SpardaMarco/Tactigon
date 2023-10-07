:- use_module(library(between)).

% clear_console/0
% Clears console
clear_screen:- 
    write('\33\[2J').

% read_number_input(-Number)
% Reads a number from user input, unifying it with Number
read_number_input(X):-
    read_number_aux(X,0).
read_number_input_aux(X,ACC):- 
    get_code(C),
    between(48, 57, C), % 48 is ASCII code for 0 and 57 is ASCII code for 9
    !,
    ACC1 is 10 * ACC + (C - 48),
    read_number_input_aux(X, ACC1).
read_number_input_aux(X,X).

% get_option(+MinValue,+MaxValue,-Option)
% Unifies Option with the value given by user input between Min and Max
get_option(MinValue, MaxValue, Option):-
    format('Select between ~d and ~d: ', [MinValue, MaxValue]),
    repeat,
    read_number(Option),
    between(MinValue, MaxValue, Option), 
    !.