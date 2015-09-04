% Deck class %
:- module( deck,
  [decks/1,
   shuffleMode/1,
   deck/1,
   shuffle/3,
   initDeal/3,
   score/2,
   getPiles/3,
   addCard/5]
  ).

:- use_module(blackJack). % players

%% define number of decks
decks(1).
%% decide whether shuffle is made with coin toss or it is deterministic
%% shuffleMode(deterministic). % deterministic
shuffleMode(random). % random

%% define suits
%% clubs (♣), diamonds (♦), hearts (♥) and spades (♠) 
%% suits(d).
suits(♦).
%% suits(h).
suits(♥).
%% suits(s).
suits(♠).
%% suits(c).
suits(♣).

%% define ranks
ranks(  ace).
ranks(    2).
ranks(    3).
ranks(    4).
ranks(    5).
ranks(    6).
ranks(    7).
ranks(    8).
ranks(    9).
ranks(   10).
ranks( jack).
ranks(queen).
ranks( king).
%% ace   :- 1; 11.
%% jack  :-    10.
%% queen :-    10.
%% king  :-    10.

% functions definitions
%% card representation
%% c( suit, rank ).
card( Suit, Rank ) :-
	suits( Suit ), ranks( Rank ).

%% Get the value of card
value( Value, card( S, R ) ) :-
	( S = ♣; S = ♦; S = ♥; S = ♠ ), !,
	( R = ace   -> ( Value is 11 ; Value is 1 )
	; R = jack  -> Value is 10
	; R = queen -> Value is 10
	; R = king  -> Value is 10
	; otherwise -> Value is R
	).

%% calculate card value | Hand is a list of *card* objects
score( Score, Hand ) :- % Hand
	score( Score, Hand, 0 ).
score( Score, [Card|Hand], V ) :-
	value(Q, Card),
	Vn is V + Q,
	score(Score, Hand, Vn).
score( Score, [], V ) :-
	Score is V.

%% generate predefined number deck---return a list with all cards
deck(Deck) :-
	decks(No),
	deck(Deck, [], No).
deck(Deck, Holder, 0) :-
	append(Holder, [], Deck),
	!.
deck(Deck, Holder, No) :-
	Np is No - 1,
	findall(card(S,R), card(S, R), New),
	append(Holder, New, NewHolder),
	deck(Deck, NewHolder, Np).

%% Return list without element E
woN(Out, E, In) :-
	select(E, In, Out), !.

%% deal the cards to N players---we begin with two cards each + shuffler
%%  + player(/no-player mode)
%% Return list of lists with cards---hands
%% % hand(L) :- [ card(), card(), card(), card(),... ]
initDeal(Table, NewDeck, Deck) :-
	players(P),
	userPlayer(Q),
	AtTable is P + 1 + Q,
	%% CardDeals is AtTable * 2, % deal 2 cards for each player
	initDeal(Table, NewDeck, [], Deck ,Deck, AtTable, 1).
initDeal(Table, Deck, Table, Deck, _, AtTable, Current) :-
	AtTable is Current - 1, !.
initDeal(Table, NewDeck, TemporaryTable, DissortingD, Deck, AtTable, Current) :-
	nth1(Current, Deck, C1), % draw element N
	Current1 is Current + AtTable, % get new index
	nth1(Current1, Deck, C2), % draw next card
	woN(Deck1, C1, DissortingD), % delete first card
	woN(Deck2, C2, Deck1), % delete first card
	Next is Current + 1, % new current player
	append([C1], [C2], Person), % create one hand
	append(TemporaryTable, [Person], Table2),% generate i-th player
	initDeal(Table, NewDeck, Table2, Deck2, Deck, AtTable, Next).

%% DEFINE COUPLE OF SHUFFLE MODES
%% shuffle a deck by RIFLE-shuffle split into half and toss a coin
%% for each card to decide whether it goes on the bottom or on the top
%% or just do it in the right order
shuffle(Shf, Shf, 0) :-
	!.
shuffle(Shuffled, Deck, N) :-
	shuffleMode(Mode),
	proper_length(Deck, Len),
	Half is Len / 2,
	getPiles(A, _, Half),
	A1 is A + 1,
	split(P1, P2, Deck, A1), % get two piles
	% random or deterministic
	( Mode = random        -> rifleRan(Forward, P1, P2)
	; Mode = deterministic -> rifleDet(Forward, P1, P2)
	),
	N1 is N - 1,
	shuffle( Shuffled, Forward, N1 ).

%% rifle shuffle two piles deterministically
rifleDet(Out, A, B) :-
	random(0, 2, Rand), % decide whether left pile goes on top or bottom,
	( Rand = 0 -> rifleDet(Out, [], A, B)
	; Rand = 1 -> rifleDet(Out, [], B, A)
	).
rifleDet(Out, Em, [A1|A2], [B1|B2]) :-
	append(Em, [A1], O1),
	append(O1, [B1], O2),
	rifleDet(Out, O2, A2, B2).
rifleDet(Out, Em, [], [B1|B2]) :-
	append(Em, [B1], O),
	rifleDet(Out, O, [], B2).
rifleDet(Out, Em, [A1|A2], []) :-
	append(Em, [A1], O),
	rifleDet(Out, O, A2, []).
rifleDet(Out, Out, [], []) :-
	!.

%% rifle shuffle two piles randomly
rifleRan(Out, A, B) :-
	random(0, 2, Rand), % decide whether left pile goes on top or bottom
	rifleRan(Out, [], A, B, Rand).
rifleRan(Out, Em, [A1|A2], [B1|B2], 0) :-
	append(Em, [A1], O1),
	append(O1, [B1], O2),
	random(0, 2, Rand),
	rifleRan(Out, O2, A2, B2, Rand).
rifleRan(Out, Em, [A1|A2], [B1|B2], 1) :-
	append(Em, [B1], O1),
	append(O1, [A1], O2),
	random(0, 2, Rand),
	rifleRan(Out, O2, A2, B2, Rand).
rifleRan(Out, Em, [], [B1|B2], Rand) :-
	append(Em, [B1], O),
	rifleRan(Out, O, [], B2, Rand).
rifleRan(Out, Em, [A1|A2], [], Rand) :-
	append(Em, [A1], O),
	rifleRan(Out, O, A2, [], Rand).
rifleRan(Out, Out, [], [], _) :-
	!.

%% generate two piles counters
getPiles(A, B, Half) :-
	float(Half),
	random(0,2,Rand), % decide whether round up left or right pile
	( Rand = 0 -> A is Half - 0.5, B is Half + 0.5
	; Rand = 1 -> A is Half + 0.5, B is Half - 0.5
	),
	!.
getPiles(Half, Half, Half) :-
	integer(Half).

%% Deal additional card to player *i*
addCard(NewDeal, NewDeck, [Card|NewDeck], OldDeal, PlayerNo) :-
	split(A, [X|C], OldDeal, PlayerNo), % get sublist
	append(X, [Card], Y), % extend sublist
	append(A, [Y], Alpha), % put at the same place new list
	append(Alpha, C, NewDeal). % put at the same place new list
