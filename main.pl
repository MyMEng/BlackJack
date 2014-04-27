%% Main function---program engine and fuel
% check load
:- ensure_loaded(library(real)).
:- consult(deck).
:- consult(player).
:- consult(strategy).
:- consult('BFS.pl').

%% change for breath-first-search---to check all possibilities at given round

% play the game
play :-
	deck(Deck), % generate deck
	initShuffles(N),
	shuffle(Shuffled, Deck, N), % initial deck shuffle
	initDeal(Table, NewDeck, Shuffled), % deal the table
	write('Initial table state:'), nl,
	printGame(Table),
	theGame(Table, NewDeck).

theGame(Table, Deck) :-
	userPlayer(U),
	% what if there is no player
	((	U = 1,
		getNoPlayers(R), % get player ID
		elementN(Elem, Allowence, R),
		( \+ (Elem = -1)  -> askCard(Answer) % ask if player has not already lost
		; otherwise       -> Answer = 0
		),
		( Answer = 1 -> addCard(NewTable, NewDeck, Deck, Table, R)
		; Answer = 0 -> ( NewTable = Table, NewDeck = Deck )
		)
	 ) ; (
	 	U = 0,
	 	NewTable = Table,
	 	NewDeck = Deck
	 )
	),
	printGame(NewTable),
	playAI(NTable, NDeck, NewTable, NewDeck), % do the AI magic
	croupierAI(NNTable, NNDeck, NTable, NDeck), % do the AI magic
	\+ checkTheEnd( Allowence ), % for the moment end the game-normally shuffle and new deal

	% Failure Driven Loops
	%% checkBJ(Allowence, _, Table), % check for initial BlackJack
	failDriven(Allowence, Table),

	theGame(NNTable, NNDeck).

% Failure Driven Loop
failDriven(Y, X) :-
	checkBJ(Y, _, X),
	%% write(Y), nl,
	fail.
failDriven(_, _) :-
	true.

% check whether all players are done playing
checkTheEnd( [ -1|Aa ] ) :-
	checkTheEnd( Aa ).
checkTheEnd( [] ).
