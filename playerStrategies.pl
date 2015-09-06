/** <module> Player strategies for the Blackjack game simulator

This module contains player strategies configuration and predicates.

@author Kacper Sokol
@license GPL
*/

:- module( playerStrategies,
  [playAI/6]
  ).

:- ensure_loaded(library(aggregate)).
:- ensure_loaded(library(real)).
:- use_module(blackJack).
:- use_module(deck).

%% dealType(-Type, +Player) is det.
%
% Check the hand type of given player:
%  * *hard* - if it does not contain any ace;
%  * *soft* - if it contains at least one ace.
%
% @param Type    The type of the player's hand: `hard` or `soft`.
% @param Player  Player's cards: a list of `card` predicates.
%
dealType(Type, Player) :-
	% count the number of aces
	aggregate_all(count, member(card(_,ace), Player), Aces),
	( Aces = 0  -> Type = hard 
	; otherwise -> Type = soft
	).

%% dealerFaceUp(-ValueR, +Hand) is det.
%
% Checks value of face-up card (2nd card) - used mainly with dealer's hand.
%
% @param ValueR  The value of face-up card (2nd card).
% @param Hand    Player's cards: a list of `card` predicates.
%
dealerFaceUp( ValueR, [ _, card(_, Value)| [] ] ) :-
	( Value = jack  -> ValueR is 10
	; Value = queen -> ValueR is 10
	; Value = king  -> ValueR is 10
	; otherwise     -> ValueR = Value
	).

%% cardsOnTable(-N, +Table) is det.
%
% Counts the number of cards on the table.
%
% @param N      The number of all cards on the table.
% @param Table  List of cards held by all player (list of lists).
%
cardsOnTable(N, Table) :-
	cardsOnTable(N, 0, Table).
%
cardsOnTable(N, N, []) :- !.
cardsOnTable(N, Accum, [T|Table]) :-
	length(T, L),
	A1 is Accum + L,
	cardsOnTable(N, A1, Table).

%% attach(-NLs, +Ls, +Elem, +X) is det.
%
% Attaches element `Elem` to list `Ls` at position `X`; where the position is 
% an index counted from 1.
%
% @param NLs   The output list with attached element.
% @param Ls    The input list to which the new element should be attached.
% @param Elem  The element to be attached.
% @param X     The position's index (starting at 1) at which the element is 
%              attached.
%
attach( [Elem|Ls], Ls, Elem, 1) :- !.
attach( [Elem], [], Elem, _) :- !.
attach( NLs, [First|Ls], Elem, X) :-
	attach(NLs, [First], Ls, Elem, X, 2).
%
attach(NLs, Head, Tail, Elem, X, X) :-
	append(Head, [Elem|Tail] , NLs), !.
attach(NLs, Head, [Bg|Tail], Elem, X, Current) :-
	Current1 is Current + 1,
	append(Head, [Bg] , NewHead),
	attach(NLs, NewHead, Tail, Elem, X, Current1).

%% replace(-NLs, +Ls, +Elem, +X) is det.
%
% Replaces the element with index `X` (counting from 1) in the list `Ls` with 
% new element `Elem`. The original element is disregarded.
%
% @param NLs   The output list with replaced element.
% @param Ls    The input list in which the old element is replaced with the new 
%              one.
% @param Elem  The new element that is placed in the list.
% @param X     The position's index (starting at 1) at which the element is 
%              replaced.
%
replace( [Elem|Ls], [_|Ls], Elem, 1) :- !.
replace( [Elem], [], Elem, _) :- !.
replace( NLs, [First|Ls], Elem, X) :-
	replace(NLs, [First], Ls, Elem, X, 2).
%
replace(NLs, Head, [_|Tail], Elem, X, X) :-
	append(Head, [Elem|Tail], NLs), !.
replace(NLs, Head, [Bg|Tail], Elem, X, Current) :-
	Current1 is Current + 1,
	append(Head, [Bg] , NewHead),
	replace(NLs, NewHead, Tail, Elem, X, Current1).

%% playAI(-NTable, -NNDeck, -NRefuse, +Table, +NDeck, +Refuse) is det.
%
% Assigns strategy to each player via `playAISequence` predicate and return 
% resulting: table, deck, refusal list.
%
% @param NTable   *New* list of cards held by all player after current round.
% @param NNDeck   *New* list of cards remaining in the deck after current round.
% @param NRefuse  *New* list of players not allowed to draw a card after 
%                 current round.
% @param Table    List of cards held by all player before current round.
% @param NDeck    List of cards remaining in the deck before current round.
% @param Refuse   List of players not allowed to draw a card before current 
%                 round.
%
playAI([Dealer|NTable1], NNDeck, NRefuse, [Dealer|NTable], NDeck, Refuse) :-
	players(X),
	playAISequence(NTable1, NNDeck, NRefuse, NTable, NDeck, Dealer, 1, X, Refuse).

%% playAISequence(-NewTable, -NNDeck, -NRefuse, +Table, +NDeck, +Dealer, +Counter, +X, +Refuse) is det.
%
% Assigns strategy to each player and simulates then; then returns resulting: 
% table, deck, refusal list. The first player in the list plays `playAIDet`, 
% the second `playAIDeck`, the third `playAIMix`, the fourth *always holds*, 
% finally the rest of the players are playing `playAIDet` strategy.
%
% @param NewTable  *New* list of cards held by all players **excluding dealer** 
%                  after current round.
% @param NNDeck    *New* list of cards remaining in the deck after current round.
% @param NRefuse   *New* list of players not allowed to draw a card after 
%                  current round.
% @param Table     List of cards held by all players *excluding dealer** before 
%                  current round.
% @param NDeck     List of cards remaining in the deck before current round.
% @param Dealer    Dealer's cards: a list of `card` predicates.
% @param Counter   The loop counter.
% @param X         The total number of AI driven players.
% @param Refuse    List of players not allowed to draw a card before current 
%                  round.
%
playAISequence(Table, Deck, Refuse, Table, Deck, _, X1, X, Refuse) :-
	X1 is X +1,
	!.
playAISequence(NewTable, NNDeck, NRefuse, Table, NDeck, Dealer, Counter, X, Refuse) :-
	% player B is,
	nth1(Counter, Table, PlayerB),

	% do different strategy for each player
	nth1(Counter, Refuse, Status),
	( Status = 0 ->
		( Counter = 1 -> ( playAIDet( PlayerA, ChDeck, PlayerB, NDeck, Dealer ),
						   replace( NewRefuse, Refuse, 1, Counter) )
		; Counter = 2 -> ( playAIDeck( PlayerA, ChDeck, PlayerB, NDeck, Table), NewRefuse = Refuse )
		; Counter = 3 -> ( playMix( PlayerA, ChDeck, PlayerB, NDeck, Dealer, Table ),
		                   replace( NewRefuse, Refuse, 1, Counter) )
		; Counter = 4 -> ( PlayerA = PlayerB, ChDeck = NDeck, replace( NewRefuse, Refuse, 1, Counter) )
		; otherwise   -> ( playAIDet( PlayerA, ChDeck, PlayerB, NDeck, Dealer ),
						   replace( NewRefuse, Refuse, 1, Counter) )
		)
	; otherwise ->
		( PlayerA = PlayerB, ChDeck = NDeck, NewRefuse = Refuse )
	),

	% if AI once refused do not allow any more
	( ( PlayerA = PlayerB, Counter = 2) -> replace( NewNewRefuse, NewRefuse, 1, Counter) % refusal
	; otherwise                         -> NewNewRefuse = NewRefuse % action taken
	),

	% attach back player A.
	replace( ChTable, Table, PlayerA, Counter),

	Counter1 is Counter + 1,
	playAISequence(NewTable, NNDeck, NRefuse, ChTable, ChDeck, Dealer, Counter1, X, NewNewRefuse).

%% playAIDet(-NPlayer, -NDeck, +Player, +Deck, +Dealer) is det.
%
% 1st strategy `playAIDet`: Deterministic strategy which can only *hit* in the 
% first round.
%
% @param NPlayer  Player's hand **after** player's action.
% @param NDeck    List of cards in the deck **after** player's action.
% @param Player   Player's hand **before** player's action.
% @param Deck     List of cards in the deck **before** player's action.
% @param Dealer   Dealer's cards: a list of `card` predicates.
%
playAIDet(NPlayer, NDeck, Player, Deck, Dealer) :-
	dealType(P, Player),
	dealerFaceUp(D, Dealer),
	%% write('Dealer face: '), write(D), nl,
	score(V, Player),!, % cards value V---take only first value where aces are treated as 11
	playAIDet(Action, V, P, D),
	% take an action
	( Action = hit   -> hitDet(NPlayer, NDeck, Player, Deck)
	; Action = stand -> standDet(NPlayer, NDeck, Player, Deck)
	).
%
playAIDet(Action, V, hard, D) :-
	hardAction(Action, D, V).
playAIDet(Action, V, soft, D) :-
	V1 is V - 11,
	softAction(Action, D, V1).
%
hitDet(NPlayer, Deck, Player, [C|Deck]) :-
	append(Player, [C], NPlayer).
%
standDet(Player, Deck, Player, Deck).

%% playAIDeck(-PlayerA, -ChDeck, +PlayerB, +Deck, +Table) is det.
%
% 2nd strategy `playAIDeck`: Shuffle tracking strategy which estimates *deck 
% probabilities*. It assumes that the deck has started being in ordered; then 
% it calculates probabilities of each card occurring at given position and 
% decides to hit or stand based on the cards with highest probabilities.
%
% @param PlayerA  Player's hand **after** player's action.
% @param ChDeck   List of cards in the deck **after** player's action.
% @param PlayerB  Player's hand **before** player's action.
% @param Deck     List of cards in the deck **before** player's action.
% @param Table    List of cards held by all players *excluding dealer** before 
%                 current round (list of lists).
%
playAIDeck( PlayerA, ChDeck, PlayerB, [Dc|NDeck], Table) :- % do the AI magic
	%% decks(D),
	%% Size is 52 * D,
	%% deckMx <- 'Diagonal(Size, x=1)', % initialize Deck matrix
	% figure out pobabilities after shuffling given number of times
	initShuffles(Quantity),
	shuffleMode(Type),
	initDeckProbabilities(Deck, Quantity, Type),
	% count how many card have alredy been used and what is the next one
	cardsOnTable(N, Table),
	N1 is N + 1,
	nth1(N1, Deck, Card),
	append(PlayerB, [Card], TrialHand),
	scoreTop(Top, TrialHand),
	( Top =< 21 -> (append(PlayerB, [Dc], PlayerA), ChDeck = NDeck)
	; otherwise -> (PlayerB = PlayerA, append([Dc], NDeck, ChDeck))
	).

%% initDeckProbabilities(-Deck, +Quantity, +Type) is det.
%
% Simulate card shuffle by preparing deck probability table for initial number 
% of shuffles. The type of shuffle can be one of those defined by `shuffleMode` 
% parameter in `deck` package. The binary vector (list of 0's and 1's) that 
% decides probability of given card position in shuffle is estimated based on 
% shuffle type.
%
% @param Deck      A shuffled initial deck of cards (list of predicates `card`).
% @param Quantity  Total umber of shuffles to do.
% @param Type      Type of card shuffle to simulate.
%
initDeckProbabilities(Deck, Quantity, random) :-
	deck(DeckL), % generate deck
	initShuffles(Quantity),
	% generate x randm vectors 0/1 each corresponding to 1 shuffle
	decks(Dno),
	( Dno=1     -> Size is 27 * Quantity
	; otherwise -> (Mp is Dno-1, SizeA is 26*Mp, SizeB is 27+SizeA, Size is SizeB*Quantity)
	),
	Sl <- sample(0:1, Size, replace=1),
	shuffleMock(Deck, DeckL, Quantity, Sl).
initDeckProbabilities(Deck, Quantity, deterministic) :- % first from the top pile is top/././.
	deck(DeckL), % generate deck
	initShuffles(Quantity),
	findall(Shuffled, shuffleMock(Shuffled, DeckL, Quantity, []), Y),
	length(Y, Len),
	El <- sample(1:Len, 1),
	nth1(El, Y, Deck). % initial deck shuffle.

%% shuffleMock(-Shuffled, +Deck, +N, +Sampling) is det.
%
% Simulate card shuffle; the type of shuffle to simulate is taken from system 
% settings: see `shuffleMode` parameter in `deck` package.
%
% @param Shuffled  Shuffled deck of cards (list of predicates `card`).
% @param Deck      A deck of cards to be shuffled (list of predicates `card`).
% @param N         Shuffling counter - number of shuffles to do.
% @param Sampling  A binary vector (list of 0's and 1's) that decides whether 
%                  card from given  pile (pile A) should be placed on top or 
%                  bottom of the card from other pile (pile B).
%
shuffleMock(Shf, Shf, 0, []) :- !.
shuffleMock(Shuffled, Deck, N, Sampling) :-
	shuffleMode(Mode),
	proper_length(Deck, Len),
	Half is Len / 2,
	getPiles(A, _, Half),
	A1 is A + 1,
	split(P1, P2, Deck, A1), % get two piles
	% random or deterministic
	( Mode = random        -> rifleRanMock(Forward, P1, P2, NSampling, Sampling)
	; Mode = deterministic -> (rifleDetMock(Forward, P1, P2), NSampling = Sampling )
	),
	write('=====\n'),write(Sampling),write('====='),write(NSampling),write('\n'),
	N1 is N - 1,
	shuffleMock( Shuffled, Forward, N1, NSampling ).

%% rifleDetMock(-Out, +A, +B) is det.
%
% Rifle-shuffle two piles of cards deterministically. The only random bit here 
% is whether the shuffle starts with pile A or pile B.
%
% @param Out        Merged deck (list of predicates `card`).
% @param A          1st part of deck to be merged (list of predicates `card`).
% @param B          2nd part of deck to be merged (list of predicates `card`).
%
rifleDetMock(Out, A, B) :-
	%% random(0, 2, Rand), % decide whether left pile goes on top or bottom,
	( Rand = 0 ; Rand = 1 ),
	( Rand = 0 -> rifleDetMock(Out, [], A, B)
	; Rand = 1 -> rifleDetMock(Out, [], B, A)
	).
%
rifleDetMock(Out, Em, [A1|A2], [B1|B2]) :-
	append(Em, [A1], O1),
	append(O1, [B1], O2),
	rifleDetMock(Out, O2, A2, B2).
rifleDetMock(Out, Em, [], [B1|B2]) :-
	append(Em, [B1], O),
	rifleDetMock(Out, O, [], B2).
rifleDetMock(Out, Em, [A1|A2], []) :-
	append(Em, [A1], O),
	rifleDetMock(Out, O, A2, []).
rifleDetMock(Out, Out, [], []) :-
	!.

%% rifleRanMock(-Out, +A, +B, -NSampling, +Sampling) is det.
%
% Rifle-shuffle two piles of cards with *random* card selection.
%
% @param Out        Merged deck (list of predicates `card`).
% @param A          1st part of deck to be merged (list of predicates `card`).
% @param B          2nd part of deck to be merged (list of predicates `card`).
% @param NSampling  See `Sampling` parameter; the leftovers needed for next 
%                   round of shuffling.
% @param Sampling   A binary vector (list of 0's and 1's) that decides whether 
%                   card from given  pile (pile A) should be placed on top or 
%                   bottom of the card from other pile (pile B).
%
rifleRanMock(Out, A, B, NSampling, [Rand| Sampling]) :-
	%% random(0, 2, Rand), % decide whether left pile goes on top or bottom
	%% ( Rand = 0 ; Rand = 1 ),
	%% write(Rand),
	rifleRanMock(Out, [], A, B, Rand, NSampling, Sampling).
%
rifleRanMock(Out, Em, [A1|A2], [B1|B2], 0, NSampling, [Rand| Sampling]) :-
	append(Em, [A1], O1),
	append(O1, [B1], O2),
	%% random(0, 2, Rand),
	%% ( Rand = 0 ; Rand = 1 ),
	%% write(Rand),
	rifleRanMock(Out, O2, A2, B2, Rand, NSampling, Sampling).
rifleRanMock(Out, Em, [A1|A2], [B1|B2], 1, NSampling, [Rand| Sampling]) :-
	append(Em, [B1], O1),
	append(O1, [A1], O2),
	%% random(0, 2, Rand),
	%% ( Rand = 0 ; Rand = 1 ),
	%% write(Rand),
	rifleRanMock(Out, O2, A2, B2, Rand, NSampling, Sampling).
rifleRanMock(Out, Em, [], [B1|B2], Rand, NSampling, Sampling) :-
	append(Em, [B1], O),
	rifleRanMock(Out, O, [], B2, Rand, NSampling, Sampling).
rifleRanMock(Out, Em, [A1|A2], [], Rand, NSampling, Sampling) :-
	append(Em, [A1], O),
	rifleRanMock(Out, O, A2, [], Rand, NSampling, Sampling).
rifleRanMock(Out, Out, [], [], _, NSampling, NSampling) :-
	!.

%% playMix(-PlayerA, -ChDeck, +PlayerB, +NDeck, +Dealer, +Table) is det.
%
% 3rd strategy `playMix`: 0.5 chance of playing *deterministic* strategy (1st) and 
% 0.5 chance of playing *deck memory* strategy (2nd).
%
% @param PlayerA  Player's hand **after** player's action.
% @param ChDeck   List of cards in the deck **after** player's action.
% @param PlayerB  Player's hand **before** player's action.
% @param NDeck    List of cards in the deck **before** player's action.
% @param Dealer   Dealer's cards: a list of `card` predicates.
% @param Table    List of cards held by all players *excluding dealer** before 
%                 current round (list of lists).
%
playMix( PlayerA, ChDeck, PlayerB, NDeck, Dealer, Table ) :-
	playAIDet( PlayerAa, ChDeckA, PlayerB, NDeck, Dealer ),
	playAIDeck( PlayerAb, ChDeckB, PlayerB, NDeck, Table),
	random(0, 2, Rand),
	( (PlayerAa=PlayerAb, ChDeckA=ChDeckB) -> (PlayerA=PlayerAa, ChDeck=ChDeckA)
	; Rand=0                               -> (PlayerA=PlayerAa, ChDeck=ChDeckA)
	; Rand=1                               -> (PlayerA=PlayerAb, ChDeck=ChDeckB)
	).

%% hardAction(-Action, +DealerCard, +PlayersScore) is det.
%% hardAction(+Action, +DealerCard, +PlayersScore) is det.
%
% A lookup table for *hard totals* strategy. For pairs the **split** action is 
% disregarded.
%
% @param Action        An action to be taken according to strategy, dealer's 
%                      hand and player's score.
% @param DealerCard    The value of dealer's card facing up.
% @param PlayersScore  The player's score.
%
hardAction(hit, 2,  4).
hardAction(hit, 2,  5).
hardAction(hit, 2,  6).
hardAction(hit, 2,  7).
hardAction(hit, 2,  8).
hardAction(hit, 2,  9).
hardAction(hit, 2, 10).
hardAction(hit, 2, 11).
hardAction(hit, 2, 12).
hardAction(stand, 2, 13).
hardAction(stand, 2, 14).
hardAction(stand, 2, 15).
hardAction(stand, 2, 16).
hardAction(stand, 2, 17).
hardAction(stand, 2, 18).
hardAction(stand, 2, 19).
hardAction(stand, 2, 20).
%
hardAction(hit, 3,  4).
hardAction(hit, 3,  5).
hardAction(hit, 3,  6).
hardAction(hit, 3,  7).
hardAction(hit, 3,  8).
hardAction(hit, 3,  9).
hardAction(hit, 3, 10).
hardAction(hit, 3, 11).
hardAction(hit, 3, 12).
hardAction(stand, 3, 13).
hardAction(stand, 3, 14).
hardAction(stand, 3, 15).
hardAction(stand, 3, 16).
hardAction(stand, 3, 17).
hardAction(stand, 3, 18).
hardAction(stand, 3, 19).
hardAction(stand, 3, 20).
%
hardAction(hit, 4,  4).
hardAction(hit, 4,  5).
hardAction(hit, 4,  6).
hardAction(hit, 4,  7).
hardAction(hit, 4,  8).
hardAction(hit, 4,  9).
hardAction(hit, 4, 10).
hardAction(hit, 4, 11).
hardAction(stand, 4, 12).
hardAction(stand, 4, 13).
hardAction(stand, 4, 14).
hardAction(stand, 4, 15).
hardAction(stand, 4, 16).
hardAction(stand, 4, 17).
hardAction(stand, 4, 18).
hardAction(stand, 4, 19).
hardAction(stand, 4, 20).
%
hardAction(hit, 5,  4).
hardAction(hit, 5,  5).
hardAction(hit, 5,  6).
hardAction(hit, 5,  7).
hardAction(hit, 5,  8).
hardAction(hit, 5,  9).
hardAction(hit, 5, 10).
hardAction(hit, 5, 11).
hardAction(stand, 5, 12).
hardAction(stand, 5, 13).
hardAction(stand, 5, 14).
hardAction(stand, 5, 15).
hardAction(stand, 5, 16).
hardAction(stand, 5, 17).
hardAction(stand, 5, 18).
hardAction(stand, 5, 19).
hardAction(stand, 5, 20).
%
hardAction(hit, 6,  4).
hardAction(hit, 6,  5).
hardAction(hit, 6,  6).
hardAction(hit, 6,  7).
hardAction(hit, 6,  8).
hardAction(hit, 6,  9).
hardAction(hit, 6, 10).
hardAction(hit, 6, 11).
hardAction(stand, 6, 12).
hardAction(stand, 6, 13).
hardAction(stand, 6, 14).
hardAction(stand, 6, 15).
hardAction(stand, 6, 16).
hardAction(stand, 6, 17).
hardAction(stand, 6, 18).
hardAction(stand, 6, 19).
hardAction(stand, 6, 20).
%
hardAction(hit, 7,  4).
hardAction(hit, 7,  5).
hardAction(hit, 7,  6).
hardAction(hit, 7,  7).
hardAction(hit, 7,  8).
hardAction(hit, 7,  9).
hardAction(hit, 7, 10).
hardAction(hit, 7, 11).
hardAction(hit, 7, 12).
hardAction(hit, 7, 13).
hardAction(hit, 7, 14).
hardAction(hit, 7, 15).
hardAction(hit, 7, 16).
hardAction(stand, 7, 17).
hardAction(stand, 7, 18).
hardAction(stand, 7, 19).
hardAction(stand, 7, 20).
%
hardAction(hit, 8,  4).
hardAction(hit, 8,  5).
hardAction(hit, 8,  6).
hardAction(hit, 8,  7).
hardAction(hit, 8,  8).
hardAction(hit, 8,  9).
hardAction(hit, 8, 10).
hardAction(hit, 8, 11).
hardAction(hit, 8, 12).
hardAction(hit, 8, 13).
hardAction(hit, 8, 14).
hardAction(hit, 8, 15).
hardAction(hit, 8, 16).
hardAction(stand, 8, 17).
hardAction(stand, 8, 18).
hardAction(stand, 8, 19).
hardAction(stand, 8, 20).
%
hardAction(hit, 9,  4).
hardAction(hit, 9,  5).
hardAction(hit, 9,  6).
hardAction(hit, 9,  7).
hardAction(hit, 9,  8).
hardAction(hit, 9,  9).
hardAction(hit, 9, 10).
hardAction(hit, 9, 11).
hardAction(hit, 9, 12).
hardAction(hit, 9, 13).
hardAction(hit, 9, 14).
hardAction(hit, 9, 15).
hardAction(hit, 9, 16).
hardAction(stand, 9, 17).
hardAction(stand, 9, 18).
hardAction(stand, 9, 19).
hardAction(stand, 9, 20).
%
hardAction(hit, 10,  4).
hardAction(hit, 10,  5).
hardAction(hit, 10,  6).
hardAction(hit, 10,  7).
hardAction(hit, 10,  8).
hardAction(hit, 10,  9).
hardAction(hit, 10, 10).
hardAction(hit, 10, 11).
hardAction(hit, 10, 12).
hardAction(hit, 10, 13).
hardAction(hit, 10, 14).
hardAction(hit, 10, 15).
hardAction(hit, 10, 16).
hardAction(stand, 10, 17).
hardAction(stand, 10, 18).
hardAction(stand, 10, 19).
hardAction(stand, 10, 20).
%
hardAction(hit, ace,  4).
hardAction(hit, ace,  5).
hardAction(hit, ace,  6).
hardAction(hit, ace,  7).
hardAction(hit, ace,  8).
hardAction(hit, ace,  9).
hardAction(hit, ace, 10).
hardAction(hit, ace, 11).
hardAction(hit, ace, 12).
hardAction(hit, ace, 13).
hardAction(hit, ace, 14).
hardAction(hit, ace, 15).
hardAction(hit, ace, 16).
hardAction(stand, ace, 17).
hardAction(stand, ace, 18).
hardAction(stand, ace, 19).
hardAction(stand, ace, 20).

%% softAction(-Action, +DealerCard, +PlayersScore) is det.
%% softAction(+Action, +DealerCard, +PlayersScore) is det.
%
% A lookup table for *soft totals* strategy. For pairs the **split** action is 
% disregarded.
%
% @param Action        An action to be taken according to strategy, dealer's 
%                      hand and player's score.
% @param DealerCard    The value of dealer's card facing up.
% @param PlayersScore  The player's score.
%
softAction(hit,   2, 2).
softAction(hit,   2, 3).
softAction(hit,   2, 4).
softAction(hit,   2, 5).
softAction(hit,   2, 6).
softAction(stand, 2, 7).
softAction(stand, 2, 8).
softAction(stand, 2, 9).
softAction(hit, 2, 11).%ace
%
softAction(hit,   3, 2).
softAction(hit,   3, 3).
softAction(hit,   3, 4).
softAction(hit,   3, 5).
softAction(hit,   3, 6).
softAction(stand, 3, 7).
softAction(stand, 3, 8).
softAction(stand, 3, 9).
softAction(hit, 3, 11).%ace
%
softAction(hit,   4, 2).
softAction(hit,   4, 3).
softAction(hit,   4, 4).
softAction(hit,   4, 5).
softAction(hit,   4, 6).
softAction(stand, 4, 7).
softAction(stand, 4, 8).
softAction(stand, 4, 9).
softAction(hit, 4, 11).%ace
%
softAction(hit,   5, 2).
softAction(hit,   5, 3).
softAction(hit,   5, 4).
softAction(hit,   5, 5).
softAction(hit,   5, 6).
softAction(stand, 5, 7).
softAction(stand, 5, 8).
softAction(stand, 5, 9).
softAction(hit, 5, 11).%ace
%
softAction(hit,   6, 2).
softAction(hit,   6, 3).
softAction(hit,   6, 4).
softAction(hit,   6, 5).
softAction(hit,   6, 6).
softAction(stand, 6, 7).
softAction(stand, 6, 8).
softAction(stand, 6, 9).
softAction(hit, 6, 11).%ace
%
softAction(hit,   7, 2).
softAction(hit,   7, 3).
softAction(hit,   7, 4).
softAction(hit,   7, 5).
softAction(hit,   7, 6).
softAction(stand, 7, 7).
softAction(stand, 7, 8).
softAction(stand, 7, 9).
softAction(hit, 7, 11).%ace
%
softAction(hit,   8, 2).
softAction(hit,   8, 3).
softAction(hit,   8, 4).
softAction(hit,   8, 5).
softAction(hit,   8, 6).
softAction(stand, 8, 7).
softAction(stand, 8, 8).
softAction(stand, 8, 9).
softAction(hit, 8, 11).%ace
%
softAction(hit,   9, 2).
softAction(hit,   9, 3).
softAction(hit,   9, 4).
softAction(hit,   9, 5).
softAction(hit,   9, 6).
softAction(hit,   9, 7).
softAction(stand, 9, 8).
softAction(stand, 9, 9).
softAction(hit, 9, 11).%ace
%
softAction(hit,   10, 2).
softAction(hit,   10, 3).
softAction(hit,   10, 4).
softAction(hit,   10, 5).
softAction(hit,   10, 6).
softAction(hit,   10, 7).
softAction(stand, 10, 8).
softAction(stand, 10, 9).
softAction(hit, 10, 11).%ace
%
softAction(hit,   ace, 2).
softAction(hit,   ace, 3).
softAction(hit,   ace, 4).
softAction(hit,   ace, 5).
softAction(hit,   ace, 6).
softAction(hit,   ace, 7).
softAction(stand, ace, 8).
softAction(stand, ace, 9).
softAction(hit,   ace, 11).%ace
