% check load
:- ensure_loaded( library(real) ).

% define constants
%% define number of decks
decks(1).

%% define suits
%% clubs (♣), diamonds (♦), hearts (♥) and spades (♠) 
%% suits(c).
suits(♣).
%% suits(d).
suits(♦).
%% suits(h).
suits(♥).
%% suits(s).
suits(♠).

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

%% card representation
%% c( suit, rank ).
card( Suit, Rank ) :-
	suits( Suit ), ranks( Rank ).

%% later on define Hand with cards
%% hand(L) :- [ card(), card(), card(), card(),... ]

%% Get the value of card
value( Value, card( S, R ) ) :-
	( R = ace   -> ( Value is 10 ; Value is 1 )
	; R = jack  -> Value is 10
	; R = queen -> Value is 10
	; R = king  -> Value is 10
	; otherwise -> Value is R
	).

%% calculate card value | Hand is a list of *card* objects
score( Score, Hand ) :- % Hand
	score( Score, Hand, 0 ).
%% score( Score, [Card|Hand], V ) :-
%% 	value(Q, Card),
%% 	Score is V + ,
%% 	pass.

% functions definitions
%% generate a deck
%% deckGen :- pass.

%% shuffle a deck by A-shuffle split into half and toss a coin
%% for each card to decide whether it goes on the bottom or on the top
%% or just do it in the right order
