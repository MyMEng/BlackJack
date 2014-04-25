% check load
:- ensure_loaded( library(real) ).

% define constants
%% define number of decks
decks(1).

%% define number of players
players(2).

%% activate interactive player or experiment mode
playerMode(interactive).
%% playerMode(experimental).


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

%% decide whether shuffle is made with coin toss or it is deterministic
shuffleMode(deterministic). % deterministic
%shuffleMode(random). % random

%% define number of shuffles after each game
shuffles(1).


% functions definitions
%% card representation
%% c( suit, rank ).
card( Suit, Rank ) :-
	suits( Suit ), ranks( Rank ).

%% Get the value of card
value( Value, card( S, R ) ) :-
	(S = ♣; S = ♦; S = ♥; S = ♠ ), !,
	( R = ace   -> ( Value is 10 ; Value is 1 )
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

%% check for user player
userPlayer(N) :-
	playerMode(M),
	userPlayer(N, M).
userPlayer(N, interactive) :-
	N is 1.
userPlayer(N, experimental) :-
	N is 0.

%% extract N-th element from a list
elementN(H, [H|_], 1) :- !.
elementN(Element, [_|T], N) :-
	NN is N - 1,
	elementN(Element, T, NN).

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
	elementN(C1, Deck, Current), % draw element N
	Current1 is Current + AtTable, % get new index
	elementN(C2, Deck, Current1), % draw next card
	woN(Deck1, C1, DissortingD), % delete first card
	woN(Deck2, C2, Deck1), % delete first card
	Next is Current + 1, % new current player
	append([C1], [C2], Person), % create one hand
	append(TemporaryTable, [Person], Table2),% generate i-th player
	initDeal(Table, NewDeck, Table2, Deck2, Deck, AtTable, Next).

%% DEFINE COUPLE OF SHUFFLE MODES
%% shuffle a deck by A-shuffle split into half and toss a coin
%% for each card to decide whether it goes on the bottom or on the top
%% or just do it in the right order
%% shuffle(Shuffled, Deck) :-
	%% shuffleMode(random),
	%% shuffleMode(deterministic),
	%% random(0,2,X).

%% Split a list into Pre-element-Post
split(A, B, List, Num) :-
	split_(A, [], List, Num),
	append(A, B, List).
split_(A, A, _, 1) :- !.
split_(A, Acum, [H|T], Num) :-
	NumN is Num - 1,
	append(Acum, [H], Sup),
	split_(A, Sup, T, NumN).

%% Deal additional card to player *i*
addCard(NewDeal, NewDeck, [Card|NewDeck], OldDeal, PlayerNo) :-
	split(A, [X|C], OldDeal, PlayerNo), % get sublist
	append(X, [Card], Y), % extend sublist
	append(A, [Y], Alpha), % put at the same place new list
	append(Alpha, C, NewDeal). % put at the same place new list
