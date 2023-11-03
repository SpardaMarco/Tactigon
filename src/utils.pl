:- use_module(library(between)).

% clear_console/0
% Clears console
clear_screen:- 
    write('\33\[2J').

% read_number_input(-Number)
% Reads a number from user input, unifying it with Number. Skips entire line if input is invalid
read_number_input(X):-
    read_number_input_aux(X, 0).

% read_number_input_aux(-Number, +Accumulator)
% Reads a number from user input, unifying it with Number. Skips entire line if input is invalid
read_number_input_aux(X, ACC):- 
    peek_code(C),
    between(48, 57, C), % 48 is ASCII code for 0 and 57 is ASCII code for 9
    get_code(_),
    !,
    ACC1 is 10 * ACC + (C - 48),
    read_number_input_aux(X, ACC1).
read_number_input_aux(X, X):-
    peek_code(10), % 10 is ASCII code for \n
    get_code(_),
    !.
read_number_input_aux(_, _) :-
    skip_line,
    !,
    fail.

% get_option(+MinValue, +MaxValue, +Objective, +Error, -Option)
% Given an objective, unifies Option with the value given by user input between Min and Max
get_option(MinValue, MaxValue, Objective, _, Option):-
    format('~a between ~d and ~d: ', [Objective, MinValue, MaxValue]),
    read_number_input(Option),
    between(MinValue, MaxValue, Option), 
    !.

get_option(MinValue, MaxValue, Objective, Error, Option):-
    format('Invalid ~a.~n', [Error]),
    get_option(MinValue, MaxValue, Objective, Error, Option).


% read_number_del(-Number, +Delimiter)
% Reads a number from user input, unifying it with Number till Delimiter. Skips entire line if input is invalid
read_number_del(X, Del):-
    read_number_del_aux(X, Del, 0).

% read_number_del_aux(-Number, +Delimiter, +Accumulator)
% Reads a number from user input, unifying it with Number till Delimiter. Skips entire line if input is invalid
read_number_del_aux(X, Del, ACC):- 
    peek_code(C),
    between(48, 57, C), % 48 is ASCII code for 0 and 57 is ASCII code for 9
    get_code(_),
    !,
    ACC1 is 10 * ACC + (C - 48),
    read_number_input_aux(X, ACC1).
read_number_del_aux(X, Del, X):-
    peek_code(Del), % 10 is ASCII code for \n
    get_code(_),
    !.
read_number_del_aux(_, _, _) :-
    skip_line,
    !,
    fail.

% get_move_input(-Coordinates)
% Reads a move from user input, in format X-Y, and unifies it with Coordinates
get_move_input(Coordinates) :-
    read_number_del_aux(X, 45),
    read_number_del_aux(Y, 10),
    Coordinates = X-Y,
    !.

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