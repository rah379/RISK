# RISK IN OCAML

Nigel Wormser (ngw37)
Teresa Huang (th458)
Rowan Hennessy (rah379)
Aidan O'Connor (ao274)

FOLLOW DIRECTIONS IN INSTALL.txt TO PLAY RISK
1. To begin, open a new Terminal shell and expand it to fill your screen
2. CD into the correct directory, as with any other project requiring make commands
3. Run ’make play’ to begin the game
4. You will be given some background on RISK:
5. Hit Enter to proceed to the game! You’ll see a message asking how many players you are playing with, as well as telling you to
enter ’quit’ at any point to quit the game.
6. Now, you enter how many players you are playing with, in the form of an ASCII Integer. However, for grading purposes entering
any number from 2-6 will suffice.
7. Now, decide which map you would like to play
8. You then will receive a message giving brief instructions on what is about to occur. In more elaborate terms, in the board
game RISK, players begin by going around in a circle, each picking a single territory they would like to start the game with
and placing one troop there, and repeating until all territories are claimed. In our version of RISK, the computer assigns all
the territories to players, as we decided as a group it would be tedious and not worth the code required to do it the way it’s
done on a physical board.
9. After entering any value, you will see the board in it’s current state, with each territory being occupied by a single troop. The
color of the territory’s name corresponds to the player in control of it, and the number after the comma corresponds to how
many troops that person has on it. So, in the picture below, Madagascar is colored Yellow, which is Player Three’s color, and
has a 1 after the comma. So, Player 3 controls Madagascar and has one troop on it.
10. At the bottom, you will see a prompt asking Player 1 to put troops on a territory. This is because in RISK, after the territories
have all been divided evenly among the players, each player has a certain amount of troops that they can place at any of their
territories, before the main game loop begins. Enter a value for how many troops you would like to place at a territory, and
watch the map update.
11. After that is complete, the game loop will begin. Each turn, players get more troops which they may place at any of their
territories. The game prompts Player 1 to do so, giving instructions on the required formatting. Territories with spaces must
be delimited with underlines, for example instead of entering ”South Africa 5”, you must enter ”South Africa 5”.
12. After Player 1 has added the troops they received, it is their turn to Attack. Player 1 can attack FROM any territory they
own, and they may attack any adjacent territory. Since this is optional, the game prompts a yes or no question asking if they
would like to attack. If the player enters ’No’, then the game moves on to the next step. If they enter ’Yes’, then the game asks
WHERE they intend to attack from. After they say where they intend to attack from, the game prompts them with where
they would like to attack. Note that you may not attack from any place with 1 troop, and if a territory has n troops, you are
only allowed to attack with ≤ n − 1 troops. After the player enters where they would like to attack, the game asks them how
many troops they would like to attack with.
13. After the player enters how many troops they would like to attack with, the game asks them to roll 1, 2,or3 dice. This is
because attacking in RISK is not a guarantee- the attacker may roll up to three dice, and the defender may roll up to 2 dice,
and then the attack outcome is based on the values of these die, in a complicated set of cases not worth including here. After
the attacking player enters how many die they would like to roll with, the defending player is asked how many die they would
like to roll with, and the attack outcome is printed to the map.
14. After attacking, the player may fortify any single location by moving n − 1 troops from one territory to another. This, again, is
optional and the game prompts the player with another yes or no question. If they say ’No’, then their turn ends and the next
player’s begins. If they say ’Yes’, then they are asked where they would like to move troops from. After they enter that, they
are asked where they would like to move troops to. Finally, they are asked how many troops they would like to move, after
they enter that value their turn ends and the next player’s turn begins.
15. To win, a player must conquer the entire world. That is, they must eliminate every other player from the game.
