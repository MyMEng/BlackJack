BlackJack card game in SWI-Prolog
=========

## Introduction
Implementation of a *BlackJack* card game in `prolog` and `r..eal` library. The later is an interface to `R` statistical package within `SWI` code.  

To run the code one needs to have `R`, `R-dev`, `SWI-Prolog` and `r..eal` installed.  

## Pre-configuration
There are number of parameters available, to customize the program. They are listed at the top of each package file.

  1. *Plays*		- Number of games to be played. (`blackJack.pl`)

  2. *Decks*		- Number of decks used in a single game. (`deck.pl`)

  3. *Players*		- Number of AI players. AI1 plays *deterministic table*, AI2 plays *deck probabilities*, AI3 plays *probabilistic choice*, AI4 plays *hold on all*. If not otherwise defined, all other players are using *deterministic table*. (`blackJack.pl`)

  3. *PlayerMode*	- The game-play can be either defined to be: *experimental* - no user interaction required, this mode is mainly used to test AI algorithms; or *interactive* - user can play along AI algorithms. (`blackJack.pl`)

  4. *ShuffleMode*	- The simulation performs *riffle shuffle*(see the report); it can be either *deterministic* -  there is only one (fair)coin toss to decide whether left or right pile starts the shuffle; or *random* - coin is tossed for each card(from left pile) to decide whether it goes on the top or on the bottom. (`deck.pl`)

  5. *InitShuffles*	- Number of deck shuffles before the game starts. (`blackJack.pl`)

  6. *Shuffles*		- Number of deck shuffles between games. (`blackJack.pl`)

  7. *Dealer*		- Dealer strategy to be used: *H17* and *S17*, both described in details in the report. (`dealerStrategies.pl`)

## Executing the program(i.e. playing BlackJack)

To run the script:

     cd ~
     git clone git@github.com:So-Cool/BlackJack.git
     cd BlackJack
     swipl

and then within SWI-Prolog:

    [blackJack].
    blackJack.

After the game is finished there will be a *score graph* and *score table*(in the `.csv` format) produced within the `BlackJack` directory. The rows in the file are scores from particular round and the columns are in order: *dealer*, *AI1-deterministic table*, *AI2-deck probabilities*, *AI3-mixture of previous two*, *AI4-always stand* and finally, if enabled, **user's** score. The same order applies to vertical bars visible in the plot.  

## Remark on point counting
The scores are win/lose based; the player score is only compared against the dealer's points:

* if the player has **more** points than the dealer and has **not exceeded** 21 only he gets a point;
* if the player has **less** points than the dealer and has **not exceeded** 21 only the dealer gets a point;
* if there is a **draw** between the player and the dealer and both have less than 21 points both get a point;
* if there is a **draw** between the player and the dealer and both have more than 21 points no one get a point;
* if the player **exceeded** 21 points and the dealer has not only the dealer gets a point;
* if the dealer exceeded 21 points and the player has not only the player gets a point.

---

**Warning!** it may become addictive **Warning!**  

---

**For more information please refer to the report available within this repository.**

---

## ToDo
- [ ] Possibility to **split** on pairs.
- [ ] Preserve the deck after each game and shuffle it instead of discarding (adds more variability):

 ```
 %% define number of shuffles after each game
shuffles(2).
```
