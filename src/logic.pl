% Combat Logic

% combat(+Attacker, +Defender, -Winner)
combat(circle, _, circle).
combat(triangle, circle, none) :- !.
combat(triangle, _, triangle).
combat(square, triangle, none) :- !.
combat(square, Defender, Winner) :- 
    Defender \= circle,
    Winner = square.

% Movements Logic
piece_moves(circle, 1).
piece_moves(triangle, 3).
piece_moves(square, 4).
piece_moves(pentagon, 5).


