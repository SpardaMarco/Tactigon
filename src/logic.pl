:- ensure_loaded('board.pl').

validate_move([Board, Player], OX-OY-DX-DY) :-
    member(position(Piece, tile(OX, OY)), Board),
    piece_info(Piece, Player, Type),
    movement(Type, Movement).


% Combat Logic

% combat(+Attacker, +Defender, -Winner)
combat(circle, _, circle).
combat(triangle, circle, none) :- !.
combat(triangle, _, triangle).
combat(square, triangle, none) :- !.
combat(square, Defender, Winner) :- 
    Defender \= circle,
    Winner = square.