/** <module> Blackjack game simulator

This module contains Blackjack game simulator: predicates and configuration. It 
is responsible for running the game and I/O interactions.

@author Kacper Sokol
@license GPL
*/

:- module( blackJack,
  [players/1,
   initShuffles/1,
   blackJack/0,
   userPlayer/1,
   scoreTop/2,
   split/4]
  ).

:- set_prolog_stack(local,  limit(4 000 000 000)).
:- ensure_loaded(library(real)).
:- use_module(deck).
:- use_module(playerStrategies).
:- use_module(dealerStrategies).

%% plays(-N) is det.
%
% Defines number of plays. Proposed values:
%  * random shuffling - 300,
%  * deterministic shuffling - 50,
%  * two decks with random shuffling 100.
% Otherwise you may run into *out of global stack* troubles.
%
% @param N  Number of plays.
%
plays(300).

%% playerMode(-Mode) is det.
%
% Defines type of play:
%  * `interactive`   - a player controlled by the user is added,
%  * `experimental`  - all players are controlled by AI algorithms as defined 
%                    in `playAISequence` predicate in `playerStrategies` module.
%
% @param Mode  Mode of play-through.
%
playerMode(experimental).

%% players(-PlayersNo) is det.
%
% Defines number of AI controlled players; the AI algorithms are assigned in 
% `playAISequence` predicate in `playerStrategies` module.
%
% @param PlayersNo  Number of AI controlled players.
%
players(4).

%% initShuffles(-N) is det.
%
% Defines number of initial shuffles: the shuffles before cards are dealt and 
% the game starts. The upper limit on this value is 10.
%
% @param N  Number of initial shuffles.
%
initShuffles(10).

%% blackJack is det.
%
% The game's main loop. The game is initialised and run based on global 
% parameters defined in header of each module.
%
blackJack :-
	<- 'library(Matrix)', % load R Matrix library
	plays(Gno), %Get nubmer of games
	getNoPlayers(Pno),
	scores <- matrix(data=0, nrow=Gno, ncol=Pno), % initialise score mx
	write('scores:'), nl,
	<- scores,
	blackJack(Gno),
	% save matrix and plot and save graph---histogram
	<- 'write.csv'(scores, file = +"./scores.csv"),
	sums <- 'colSums(scores, na.rm = FALSE, dims = 1)',
	sums[1] <- sums[1] / (Pno-1),
	<- barplot(sums),
	Bar is Gno / 2,
	<- abline(h=Bar),
	<- 'png(filename="./scores.png")',
	<- barplot(sums),
	<- abline(h=Bar),
	<- 'dev.off()'.
%
blackJack(0) :- !.
blackJack(X) :-
	play(X),
	X1 is X - 1,
	blackJack(X1).

%% play(+Gno) is det.
%
% Initialises and tun a single game.
%
% @param Gno  Number of games to be run.
%
play(Gno) :-
	deck(Deck), % generate deck
	initShuffles(N),
	shuffle(Shuffled, Deck, N), % initial deck shuffle
	initDeal(Table, NewDeck, Shuffled), % deal the table
	write('Initial table state:'), nl,
	printGame(Table, init),
	players(X), refusal(Refused, X, 0), % get list of players who refused to play
	theGame(FinalTable, Table, NewDeck, 1, Refused),
	appendScores(FinalTable, Gno).

%% appendScores(+FinalTable, +Gno) is det.
%
% Calculates the scores and puts them into R variables for graph plotting and 
% CSV exporting.
%
% @param FinalTable  The final card allocation on the table (list of lists of 
%                    `card` predicates).
% @param Gno         Game id.
%
appendScores(FinalTable, Gno) :-
	getScoreTable(Lis, FinalTable), % <- this failed <- resolved
	%% Lis = [1,2,3,4,5,6],
	write('Lis:'), nl, write(Lis), nl,
	scores[Gno,*] <- Lis.
%
getScoreTable(Lis, [Dealer|FinalTable]) :-
	scoreTop(D, Dealer),
	getScores(LisPlay, Deals, 0, [], D, FinalTable ),
	append([Deals], LisPlay, Lis).
%
getScores(Lis, Deals, Deals, Lis, _, [] ) :- !.
getScores(Lis, Deals, DCount, Accum, D, [Curr|FinalTable] ) :-
	scoreTop(C, Curr),
	( (C = D, C =< 21)  -> (append(Accum, [1], AccumPlus), NDCount is 1 + DCount )
	; (C < D, D =< 21)  -> (append(Accum, [0], AccumPlus), NDCount is 1 + DCount )
	; (C =< 21, D > 21) -> (append(Accum, [1], AccumPlus), NDCount is DCount )
	; (C > 21, D > 21)  -> (append(Accum, [0], AccumPlus), NDCount is DCount )
	; (C > D, C =< 21)  -> (append(Accum, [1], AccumPlus), NDCount is DCount )
	; (C > 21, D =< 21) -> (append(Accum, [0], AccumPlus), NDCount is 1 + DCount )
	),
	getScores(Lis, Deals, NDCount, AccumPlus, D, FinalTable ).

%% scoreTop(-Sc, +Hand) is det.
%
% Gets the highest score closest to 21 but not exceeding it if possible (ace 
% handling).
%
% @param Sc    The optimal score of the input hand.
% @param Hand  A list of player's cards (`card` predicates).
%
scoreTop(Sc, Hand) :-
	findall( S, score(S, Hand), Scores ),
	getTop(Sc, Scores).
%
getTop(Sc, [H|Scores]) :-
	getTop(Sc, H, Scores).
getTop(Sc, Sc, []).
getTop(Sc, Current, [H|Rest]) :-
	( (H > Current, H =< 21)       -> getTop(Sc, H, Rest)
	; (H < Current, Current > 21 ) -> getTop(Sc, H, Rest)
	; otherwise                    -> getTop(Sc, Current, Rest)
	).

%% theGame(-FinalTable, +Table, +Deck, +Ask, +Refused) is det.
%
% A predicate collection for a single game - the game engine.
%
% @param FinalTable  The table after the whole round.
% @param Table       The initial table - initial deal.
% @param Deck        The deck of cards to draw additional cards.
% @param Ask         Player interaction parameter: 1 - ask player for card 
%                    drawing (interactive mode only); 0 - do not ask the player.
% @param Refused     A vector of 0's (allow) and 1's (do not allow) indicating 
%                    which player is allowed to draw additional card.
%
theGame(Table, Table, _, 0, Refused) :-
	players(X),
	%% write('players: '), write(X), nl,
	%% write('refused: '), write(Refused), nl,
	refusal(RefusedOriginal, X, 1),
	RefusedOriginal = Refused,
	write('The End'), nl,
	write('================================================================================'), nl,
	%% printGame(Table, cont),
	!.
% play the game
theGame(FinalTable, Table, Deck, Ask, Refused) :-
	userPlayer(U),

	%% write('before Bj'), nl,
	checkBJ(Allowence, _, Table), % check for initial BlackJack
	%% write(Allowence), nl,
	aIbj(ReRefused, Allowence, Refused),

	% what if there is no player
	((	U = 1,
		getNoPlayers(R), % get player ID
		nth1(R, Allowence, Elem),
		( (\+ (Elem = -1), Ask = 1)  -> askCard(Answer) % ask if player has not already lost
		; otherwise                  -> Answer = 0
		),

		% do not allow to take card once you have refused

		( Answer = 1 -> (addCard(NewTable, NewDeck, Deck, Table, R), Ask1 = Ask)
		; Answer = 0 -> ( NewTable = Table, NewDeck = Deck, Ask1 is 0 )
		)
	 ) ; (
	 	Ask1 = Ask,
	 	U = 0,
	 	NewTable = Table,
	 	NewDeck = Deck
	 )
	),

	%% write('before AI'), nl,
	playAI(NTable, NDeck, NRefused, NewTable, NewDeck, ReRefused), % do the AI magic
	%% NTable=NewTable, NDeck= NewDeck, NRefused = Refused,

	%% write('before Croup'), nl,
	% if BJ do not allow`
	nth1(1, Allowence, Cru),
	( Cru =  1 -> (NNTable = NTable, NNDeck = NDeck)
	; Cru = -1 -> (NNTable = NTable, NNDeck = NDeck)
	; Cru =  0 -> croupierAI(NNTable, NNDeck, NTable, NDeck) % do the AI magic
	),

	printGame(NNTable, cont),
	%% \+ checkTheEnd( Allowence ), % for the moment end the game-normally shuffle and new deal
	%% write(NRefused), nl,

	%% if experimental mode swap
	( U = 0     -> Ask2 is 0
	; otherwise -> Ask2 is Ask1
	),

	theGame(FinalTable, NNTable, NNDeck, Ask2, NRefused).

%% aIbj(-ReRefused, +Allowance, +Refused) is det.
%
% Updates the player refusal vector - a vector which indicates which player 
% has refused a card hence is no allowed to take one any more - based on 
% player's score: if a player hits or exceeds 21 it is no more allowed to take 
% a card.
%
% @param ReRefused  Updated player refusal vector based on scores.
% @param Allowance  A vector of the current player's status in the game: -1 - 
%                   more than 21 points; 0 - less than 21 points; 1 - exactly 
%                   21 points.
% @param Refused    Players refused to take the card hence are not allowed to 
%                   play any more - a vector of 1's (refused) and 0's (not yet 
%	                  refused).
%
aIbj(ReRefused, [_|Allowence], Refused) :-
	aIbj(ReRefused, [], Allowence, Refused).
aIbj(Refused, Refused, _, []) :- !.
aIbj(Refused, Collector, [A|Allowence], [R|RRefused]) :-
	( A = 1     -> append( Collector, [A], NewCollector )
	; otherwise -> append( Collector, [R], NewCollector )
	),
	aIbj(Refused, NewCollector, Allowence, RRefused).

%% checkTheEnd(+Aa) is det.
%
% Checks whether all players are done playing.
%
% @param Aa  The players status (list of player states); see checkBJ:ScoreTabel 
%            for details.
%
checkTheEnd( [ -1|Aa ] ) :-
	checkTheEnd( Aa ).
checkTheEnd( [] ).

%% userPlayer(-N) is det.
%
% Checks whether interactive player is activated and returns 1 if so.
%
% @param N  Number of interactive players in the game: 1 or 0.
%
userPlayer(N) :-
	playerMode(M),
	userPlayer(N, M).
%
userPlayer(N, interactive) :-
	N is 1.
userPlayer(N, experimental) :-
	N is 0.

%% getNoPlayers(-R) is det.
%
% Calculates the total number of players in the game including dealer, AIs and 
% interactive player.
%
% @param R  Number of players in the game.
%
getNoPlayers(R) :-
	players(P),
	userPlayer(Q),
	R is P + 1 + Q.

%% refusal(-Ls, +X, +Content) is det.
%
% Generates a list with `X` elements `Content`.
%
% @param Ls       List populated with `X` occurrences of `Content`.
% @param X        Number of elements to be populated.
% @param Content  Element to be populated.
%
refusal([], 0, _) :- !.
refusal([Content], 1, Content) :- !.
refusal(Ls, X, Content) :-
	L = [Content],
	refusal(Ls, L, 1, X, Content).
%
refusal(L, L, X, X, _) :- !.
refusal(Ls, L, C, X, Content) :-
	C1 is C + 1,
	append(L, [Content], L1),
	refusal(Ls, L1, C1, X, Content).

%% checkBJ(-ScoreTable, -Values, +Table) is det.
%
% Checks the status of the game - instant win/loose; returns all possible 
% scores of each hand no the table.
%
% @param ScoreTable  The current player's status in the game: -1 - more than 
%                    21 points; 0 - less than 21 points; 1 - exactly 21 points.
% @param Values      The list of player's scores; each player can have more 
%                    than one score in case he has an ace in his hand.
% @param Table       List of cards held by all player (list of lists).
%
checkBJ( ScoreTable, Values, Table ) :-
	checkBJ( ScoreTable, Values, [], [], Table ).
checkBJ( ScoreTable, Values, Accum1, Accum2, [H|Table] ) :-
	findall( S, score(S, H), Scores ),
	append( Accum2, [Scores], V1 ),
	findMinBJ( Smin, Scores ),
	( Smin = 21 -> append( Accum1, [1 ], Ulated )
	; Smin < 21 -> append( Accum1, [0 ], Ulated )
	; Smin > 21 -> append( Accum1, [-1], Ulated )
	),
	checkBJ(ScoreTable, Values, Ulated, V1, Table).
checkBJ(Score, Values, Score, Values, []).

%% findMinBJ(-Smin, +Scores) is det.
%
% If 21 is in the list it is returned otherwise, the minimum of the input list 
% is returned.
%
% @param Smin    The minimum in the input list **or** 21 if a member of the 
%                list.
% @param Scores  The list of (unsorted) numbers.
%
findMinBJ( Smin, Scores ) :-
	member(21, Scores), !,
	Smin is 21.
findMinBJ( Smin, Scores ) :-
	findMin(Smin, Scores).

%% findMin(-Smin, +Scores) is det.
%
% Finds the minimum of the input list.
%
% @param Smin    The minimum in the input list.
% @param Scores  The list of (unsorted) numbers.
%
findMin( Smin, [S|Scores] ) :-
	Min is S,
	findMin(Smin, Min, Scores).
%
findMin(Smin, Min, [S|Scores]) :-
	( Min >= S  -> NewMin is S
	; otherwise -> NewMin is Min
	),
	findMin(Smin, NewMin, Scores).
findMin(S, S, []).

%% split(-A, -B, +List, +Num) is det.
%
% Split a list into two sub-lists at given index. Index can be 1:# of elements 
% in the input list. The indexing starts at 1. The specified index and all of 
% the following elements will be placed in the second list.
%
% @param A     Pre-index sub-list.
% @param B     Post-index sub-list.
% @param List  The list to be split.
% @param Num   The list to be split.
%
split(A, B, List, Num) :-
	split_(A, [], List, Num),
	append(A, B, List).
split_(A, A, _, 1) :- !.
split_(A, A, _, 1.0) :- !.
split_(A, Acum, [H|T], Num) :-
	NumN is Num - 1,
	append(Acum, [H], Sup),
	split_(A, Sup, T, NumN).

%% printGame(+Table, +Type) is det.
%
% Nicely prints out a game state. It assumes that the first player on the 
% table is the dealer.
%
% @param Table  List of cards held by all player (list of lists).
% @param Type   Type of a line(s) to be printed `initial`: the first one or `cont`: 
%               any other.
%
printGame(Table, Type) :-
	players(X), userPlayer(Y),
	Total is X + Y + 1,
	write('Table:'), nl,
	printHands(Table, Total, 1, Type).

%% printHands(+Table, +Total, +N, +Type) is det.
%
% Nicely prints out a table of cards. It assumes that the first player on the 
% table is the dealer.
%
% @param Table  List of cards held by all player (list of lists).
% @param Total  Total number of players including dealer, AIs and interactive.
% @param N      Player counter: 1-#ofPlayers.
% @param Type   Type of a line(s) to be printed `initial`: the first one or `cont`: 
%               any other.
%
printHands([H|T], Total, N, Type) :-
	userPlayer(U),
	( N = 1                -> ( write('Casino'), Restrict is 1 )
	; ( N = Total, U = 1 ) -> ( write('My'), Restrict is 0 )
	; otherwise            -> ( write('AI'), No is N - 1, write(No), Restrict is 0 )
	),
	write(':	'),
	printHand(H, Restrict, Type), nl,
	N =< Total,
	N1 is N + 1,
	printHands(T, Total, N1, Type).
printHands([], N, N1, _) :-
	N1 is N + 1.
printHands([], _).

%% printHand(+Hand, +N, +Type) is det.
%
% Nicely prints out a hand of cards. Can be used for both dealer by hiding the 
% first card or by players by showing all of the cards - see the parameters for 
% details.
%
% @param Hand  A hand of player to be printed (list of `card` predicates).
% @param N     0: hides the first card; 1: shows all the cards.
% @param Type  Type of a line to be printed `initial`: the first one or `cont`: 
%              any other.
%
printHand([card( _, _ )|T], 1, init) :-
	write('_'),
	write('_'),
	write(' '),
	printHand(T, 0, init).
printHand( D, 1, cont) :-
	printHand( D, 0, cont).
printHand([card( Suit, Rank )|T], 0, _) :-
	write(Suit),
	( Rank = ace    -> write('A')
	; Rank = jack   -> write('J')
	; Rank = queen  -> write('Q')
	; Rank = king   -> write('K')
	; integer(Rank) -> write(Rank)
	),
	write(' '),
	printHand(T, 0, _).
printHand([], _, _).

%% askCard(-Answer) is det.
%
% Asks whether the user wants to get a new card.
%
% @param Answer  Boolean valued decision variable: 1 (yes) or 0 (no).
%
askCard(Answer) :-
	write('Do you want to draw a card? [y/n]'), nl, % [121] / [110]
	read_line_to_codes(user_input, Input),
	( Input = [121] -> Answer is 1
	; Input = [110] -> Answer is 0
	; otherwise     -> askCard(Answer)
	).
