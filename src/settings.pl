% difficulty(+Player, -Difficulty)
% Gets the difficulty of the player. 1 for human player, 2 for easy, 3 for hard
:- dynamic difficulty/2.
difficulty(cian, 1).
difficulty(red, 2).

% rules(?Rules)
% Gets the rules of the game, 0 for no additional rules, 1 for the first additional rule, 2 for the second additional rule, 3 for both additional rules
:- dynamic rules/1.
rules(0).

