%% Main function---program engine and fuel
% check load
:- ensure_loaded(library(real)).
:- consult(deck).
:- consult(player).
:- consult(strategy).
:- consult(deterministicTable).

% define number of plays
plays(100).

main :-
	plays(Gno), %Get nubmer of games
	getNoPlayers(Pno),
	scores <- matrix(data=0, nrow=Gno, ncol=Pno), % initialise score mx
	write('scores:'), nl,
	<- scores,
	main(Gno),
	% save matrix and plot and save graph---histogram
	<- write..matrix(scores, file = paste(targetPath, "dat2.csv", sep="/"), sep=",").
main(0) :- !.
main(X) :-
	play(Gno),
	X1 is X - 1,
	main(X1).


% play the game
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

% put everything into R variable plots etc. and play again
appendScores(FinalTable, Gno) :-
	play.


% finish the game
theGame(Table, Table, _, 0, Refused) :-
	players(X),
	refusal(Refused, X, 1),
	write('The End'), nl,
	write('================================================================================'), nl,
	%% printGame(Table, cont),
	!.
% play the game
theGame(FinalTable, Table, Deck, Ask, Refused) :-
	userPlayer(U),

	%% write('before Bj'), nl,
	checkBJ(Allowence, _, Table), % check for initial BlackJack
	write(Allowence), nl,
	aIbj(ReRefused, Allowence, Refused),

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

	%% write('before AI'), nl,
	playAI(NTable, NDeck, NRefused, NewTable, NewDeck, ReRefused), % do the AI magic
	%% NTable=NewTable, NDeck= NewDeck, NRefused = Refused,

	%% write('before Croup'), nl,
	% if BJ do not allow`
	elementN(Cru, Allowence, 1),
	( Cru =  1 -> (NNTable = NTable, NNDeck = NDeck)
	; Cru = -1 -> (NNTable = NTable, NNDeck = NDeck)
	; Cru =  0 -> croupierAI(NNTable, NNDeck, NTable, NDeck) % do the AI magic
	),

	printGame(NNTable, cont),
	%% \+ checkTheEnd( Allowence ), % for the moment end the game-normally shuffle and new deal
	%% write(NRefused), nl,
	theGame(FinalTable, NNTable, NNDeck, Ask1, NRefused).

% check whether all players are done playing
checkTheEnd( [ -1|Aa ] ) :-
	checkTheEnd( Aa ).
checkTheEnd( [] ).

% if BJ copy to the vector
aIbj(ReRefused, [_|Allowence], Refused) :-
	aIbj(ReRefused, [], Allowence, Refused).
aIbj(Refused, Refused, _, []) :- !.
aIbj(Refused, Collector, [A|Allowence], [R|RRefused]) :-
	( A = 1     -> append( Collector, [A], NewCollector )
	; otherwise -> append( Collector, [R], NewCollector )
	),
	aIbj(Refused, NewCollector, Allowence, RRefused).
