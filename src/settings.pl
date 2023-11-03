% difficulty(+Player, -Difficulty)
% Gets the difficulty level. 1 for easy bot (valid random move), 2 for hard bot (the best play at the time) and 3 for human player.
:- dynamic difficulty/2.
difficulty(cian, 1).
difficulty(red, 2).

% rules(?Rules)
% Gets the rules of the game. 1 for the first additional rule, 2 for the second additional rule.
% Rules(4) means that the game is played without additional rules.
:- dynamic rules/1.
rules(4).