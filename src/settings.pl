% difficulty(+Player, -Difficulty)
% Gets the difficulty level. 0 for human player, 1 for easy bot (valid random move) and 2 for hard bot (the best play at the time)
:- dynamic difficulty/2.
difficulty(cian, 1).
difficulty(red, 2).

% rules(?Rules)
% Gets the rules of the game. 0 for no additional rules, 1 for the first additional rule, 2 for the second additional rules.
:- dynamic rules/1.
rules(0).