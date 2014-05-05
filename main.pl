%% Main function---program engine and fuel
% check load
:- ensure_loaded(library(real)).
:- consult(deck).
:- consult(player).
:- consult(strategy).
:- consult(deterministicTable).

%% change for breath-first-search---to check all possibilities at given round

% play the game
play :-
	deck(Deck), % generate deck
	initShuffles(N),
	shuffle(Shuffled, Deck, N), % initial deck shuffle
	initDeal(Table, NewDeck, Shuffled), % deal the table
	write('Initial table state:'), nl,
	printGame(Table, init),
	Refused = [], % get list of players who refused to play
	theGame(Table, NewDeck, 1, Refused).
	% put everything into R variable plots etc. and play again

theGame(Table, Deck, Ask, Refused) :-
	userPlayer(U),

	checkBJ(Allowence, _, Table), % check for initial BlackJack

	% what if there is no player
	((	U = 1,
		getNoPlayers(R), % get player ID
		elementN(Elem, Allowence, R),
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

	playAI(NTable, NDeck, NRefused, NewTable, NewDeck, Refused), % do the AI magic
	%% NTable=NewTable, NDeck= NewDeck, NRefused = Refused,

	croupierAI(NNTable, NNDeck, NTable, NDeck), % do the AI magic
	printGame(NNTable, cont),
	\+ checkTheEnd( Allowence ), % for the moment end the game-normally shuffle and new deal
	theGame(NNTable, NNDeck, Ask1, NRefused).

% check whether all players are done playing
checkTheEnd( [ -1|Aa ] ) :-
	checkTheEnd( Aa ).
checkTheEnd( [] ).
