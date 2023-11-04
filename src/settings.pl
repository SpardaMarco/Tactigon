% difficulty(+Player, -Difficulty)
% Gets the difficulty level of a player. 1 for the easy bot (valid random move), 2 the for hard bot (the best play at the time) and 3 for a human player.
:- dynamic difficulty/2.
difficulty(cian, 1).
difficulty(red, 2).

% rules(?Rules)
% Gets the rules of the game. 1 for the first advanced rule and 2 for the second advanced rule.
% Rules(4) means that the game is played without advanced rules.
:- dynamic rules/1.
rules(4).