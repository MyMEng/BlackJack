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
shuffleMode(d). % deterministic
%shuffleMode(r). % random


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

%% deal the cards to N players-- we begin with two cards each + shuffler
%%  + player(/no-player mode)
%% Return list of lists with cards---hands
%% % hand(L) :- [ card(), card(), card(), card(),... ]
initDeal(Table, Deck) :-
	players(P),
	userPlayer(Q),
	AtTable is N + 1 + Q,
	CardDeals is AtTable * 2,
	initDeal(Table, Deck, [], CardDeals).

initDeal(Table, Deck, [], CardDeals) :-
	pass.


%% shuffle(Shuffled, Deck) :-
	%% shuffleMode(d),
	%% true.

%% shuffle a deck by A-shuffle split into half and toss a coin
%% for each card to decide whether it goes on the bottom or on the top
%% or just do it in the right order
