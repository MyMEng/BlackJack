%% Main function---program engine and fuel
consult(deck).
consult(player).
consult(strategy).

% play the game
play :-
	deck(Deck), % generate deck
	shuffle(Shuffled, Deck), % initial deck shuffle
	initDeal(Table, NewDeck, Shuffled), % deal the table
	%% checkBJ(Allowence, Score, Table), % check for initial BlackJack
	write('Initial table state:'), nl,
	printGame(Table),
	theGame(Table, NewDeck).

theGame(Table, Deck) :- % what if there is no player???????
	checkBJ(Allowence, _, Table), % check for initial BlackJack
	write(Allowence),
	getNoPlayers(R), % get player ID
	elementN(Elem, Allowence, R),
	( \+ (Elem = -1)  -> askCard(Answer) % ask if player has not already lost
	; otherwise       -> Answer = 0
	),
	( Answer = 1 -> addCard(NewTable, NewDeck, Deck, Table, R)
	; Answer = 0 -> ( NewTable = Table, NewDeck = Deck )
	),
	printGame(NewTable),
	%%, % do the AI magic
	\+ checkTheEnd( Allowence ), % for the moment end the game-normally shuffle and new deal
	theGame(NewTable, NewDeck).

% check whether all players are done playing
checkTheEnd( [ -1|Aa ] ) :-
	checkTheEnd( Aa ).
checkTheEnd( [] ).

%% write(Score), nl, write(Allowence).
