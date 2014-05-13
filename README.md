BlackJack card game in SWI-Prolog
=========

## Introduction
Implementation of a *BlackJack* card game in `prolog` and `r..eal` library. The later is an interface to `R` statistical package within `SWI` code.  

To run the code one needs to have `R`, `R-dev`, `SWI-Prolog` and `r..eal` installed.  

## Pre-configuration
There are number of parameters available, to customize the program. They are listed at the top of `main.pl` file.

  1. *Plays*		- Number of games to be played.

  2. *Decks*		- Number of decks used in a single game.

  3. *Players*		- Number of AI players. AI1 plays *deterministic table*, AI2 plays *deck probabilities*, AI3 plays *probabilistic choice*, AI4 plays *hold on all*. If not otherwise defined, all other players are using *deterministic table*.

  3. *PlayerMode*	- The game-play can be either defined to be: *experimental* - no user interaction required, this mode is mainly used to test AI algorithms; or *interactive* - user can play along AI algorithms.

  4. *ShuffleMode*	- The simulation performs *riffle shuffle*(see the report); it can be either *deterministic* -  there is only one (fair)coin toss to decide whether left or right pile starts the shuffle; or *random* - coin is tossed for each card(from left pile) to decide whether it goes on the top or on the bottom.

  5. *InitShuffles*	- Number of deck shuffles before the game starts.

  6. *Shuffles*		- Number of deck shuffles between games.

  7. *Dealer*		- Dealer strategy to be used: *H17* and *S17*, both described in details in the report.

## Executing the program(i.e. playing BlackJack)

To run the script:

     cd ~
     git clone git@github.com:So-Cool/BlackJack.git
     cd BlackJack
     swipl

and then within SWI-Prolog:

    consult('main').
    main.

After the game is finished there will be a *score graph* and *score table*(in the `.csv` format) produced within the `BlackJack` directory. The rows in the file are scores from particular round and the columns are in order: *dealer*, *AI1-deterministic table*, *AI2-deck probabilities*, *AI3-mixture of previous two*, *AI4-always stand* and finally, if enabled, **user's** score. The same order applies to vertical bars visible in the plot.  

---

**Warning!** it may become addictive **Warning!**  

---

**For more information please refer to the report available within this repository.**