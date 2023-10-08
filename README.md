# Functional and Logic Programming - 1st Pratical Assignment
## Game Theme
**[Tactigon Board Game](https://tactigongame.com/)**
## Group Description
Group Name: **Tactigon_4**

Group Members:
- **[Marco André Pereira da Costa](https://github.com/SpardaMarco)** - up202108821 - 50% contribution
- **[Tiago Filipe Castro Viana](https://github.com/tiagofcviana)** - up201807126 - 50% contribution

## Installation and Execution
In order to install and execute the game, you must download ***PFL_TP1_T03_Tactigon_4.ZIP*** and extract it. Then, inside the src directory, consult the ***main.pl*** file through Sicstus Prolog 4.8. Finally, run the following command:
```prolog
?- play.
```
The game is available for Windows and Linux.

## Game Description

**Tactigon** is a board game for two players, played on a irregular hexagon board. The game is played with 13 pieces for each player. The game starts with a default board configuration, and the players take turns moving their pieces and resolving any combat that may result from that movement.

General movement rules:
- Pieces can move along any path and in any direction up to their maximum spaces.
- Pieces can't jump over other pieces.*
- Maximum spaces is equal to the number of sides of the piece.**

\* - This rule can be changed by applying the advanced rule 1.<br>
** - This rule can be changed by applying the advanced rule 2.

Pieces can be of four types:
- **Circle** - 1 side
- **Triangle** - 3 sides
- **Square** - 4 sides
- **Pentagon** - 5 sides


![CombatTable](img/combatTable.png)
Figure 1 - Combat Table

The pieces can combat the opponent's pieces by moving to a tile occupied by an opposing piece.

Combat has two outcomes:
- The defending piece is captured, marked by a sword icon on the Figure 1.
- Both pieces are captured, marked by a two swords crossing icon on the Figure 1.

The sword and shield icons represent certain combats that can't occur in the game.<br>
Captured pieces are removed from the board and can't be used for the rest of the game.

The victory can be achieved by **capturing the opponent's pentagon** or by occupying **both gold tiles** at the **end** of the **opponent's turn**.

Two optional advanced rules can be applied to the game:
1. *Square pieces can jump over other pieces, except for opposing squares. A "jumped" tile still counts towards the piece's move limit.*
2. *Pieces that start a turn on a gold tile can move an additional space on that turn.*

For more information about the game, please consult the [official website](https://tactigongame.com/).<br>
For more information about the game rules, please consult the [How to Play](https://tactigongame.com/how-to-play/) or [Rulebook](https://online.fliphtml5.com/hvuax/bvzo/).

