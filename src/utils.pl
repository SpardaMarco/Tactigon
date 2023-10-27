:- use_module(library(between)).

% clear_console/0
% Clears console
clear_screen:- 
    write('\33\[2J').

% read_number_input(-Number)
% Reads a number from user input, unifying it with Number
read_number_input(X):-
    read_number_input_aux(X,0).
read_number_input_aux(X,ACC):- 
    get_code(C),
    between(48, 57, C), % 48 is ASCII code for 0 and 57 is ASCII code for 9
    !,
    ACC1 is 10 * ACC + (C - 48),
    read_number_input_aux(X, ACC1).
read_number_input_aux(X,X).

% get_option(+MinValue,+MaxValue,+Objective,-Option)
% Unifies Option with the value given by user input between Min and Max given an objective
get_option(MinValue, MaxValue, Objective, Option):-
    format('~a between ~d and ~d: ', [Objective, MinValue, MaxValue]),
    repeat,
    read_number_input(Option),
    between(MinValue, MaxValue, Option), 
    !.

% Read a move from user input in format X-Y without the . in the end, and unifies it with Coordinates
get_move_input(Player, Coordinates) :-
    repeat,
    read(X-Y),
    between(0, 6, X),
    between(0, 10, Y),
    !,
    Coordinates = X-Y.

% abs(+X,-Y)
% Unifies Y with the absolute value of X
abs(X,X) :- X >= 0, !.
abs(X,Y) :- Y is -X.

% print_n(+N,+S)
% Prints S N times
print_n(0, _) :- !.

print_n(N, S) :-
    N > 0,
    write(S),
    N1 is N-1,
    !,
    print_n(N1, S).