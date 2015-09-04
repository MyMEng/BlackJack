%% Main function---program engine and fuel
:- module( blackJack,
  [players/1,
   initShuffles/1,
   blackJack/0,
   blackJack/1,
   userPlayer/1,
   scoreTop/2,
   split/4]
  ).

% check load
:- set_prolog_stack(local,  limit(4 000 000 000)).
% modules
:- ensure_loaded(library(real)).
:- use_module(deck).
:- use_module(playerStrategies).
:- use_module(dealerStrategies).

% define constants
%% define number of plays
plays(300). % random->300 | deterministic->50 | 2decks random 100
%% activate interactive player or experiment mode
%% playerMode(interactive).
playerMode(experimental).
%% define number of players
players(4).
%% define number of shuffles before game starts
initShuffles(10). %max 10


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
blackJack(0) :- !.
blackJack(X) :-
	play(X),
	X1 is X - 1,
	blackJack(X1).


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
	getScoreTable(Lis, FinalTable), % <- this failed <- resolved
	%% Lis = [1,2,3,4,5,6],
	write('Lis:'), nl, write(Lis), nl,
	scores[Gno,*] <- Lis.
getScoreTable(Lis, [Dealer|FinalTable]) :-
	scoreTop(D, Dealer),
	getScores(LisPlay, Deals, 0, [], D, FinalTable ),
	append([Deals], LisPlay, Lis).
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

% get highest score closest to 21
scoreTop(Sc, Hand) :-
	findall( S, score(S, Hand), Scores ),
	getTop(Sc, Scores).
getTop(Sc, [H|Scores]) :-
	getTop(Sc, H, Scores).
getTop(Sc, Sc, []).
getTop(Sc, Current, [H|Rest]) :-
	( (H > Current, H =< 21)       -> getTop(Sc, H, Rest)
	; (H < Current, Current > 21 ) -> getTop(Sc, H, Rest)
	; otherwise                    -> getTop(Sc, Current, Rest)
	).

% finish the game
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


%%%%%%%%



%% check for user player
userPlayer(N) :-
	playerMode(M),
	userPlayer(N, M).
userPlayer(N, interactive) :-
	N is 1.
userPlayer(N, experimental) :-
	N is 0.


%% get number of players
getNoPlayers(R) :-
	players(P),
	userPlayer(Q),
	R is P + 1 + Q.



%% generate list with N 0's
refusal([], 0, _) :- !.
refusal([Content], 1, Content) :- !.
refusal(Ls, X, Content) :-
	L = [Content],
	refusal(Ls, L, 1, X, Content).

refusal(L, L, X, X, _) :- !.
refusal(Ls, L, C, X, Content) :-
	C1 is C + 1,
	append(L, [Content], L1),
	refusal(Ls, L1, C1, X, Content).






%% check win and end game
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

findMinBJ( Smin, Scores ) :-
	member(21, Scores), !,
	Smin is 21.
findMinBJ( Smin, Scores ) :-
	findMin(Smin, Scores).

%% find minimum of a list
findMin( Smin, [S|Scores] ) :-
	Min is S,
	findMin(Smin, Min, Scores).
findMin(Smin, Min, [S|Scores]) :-
	( Min >= S  -> NewMin is S
	; otherwise -> NewMin is Min
	),
	findMin(Smin, NewMin, Scores).
findMin(S, S, []).





%% Split a list into Pre-element-Post
split(A, B, List, Num) :-
	split_(A, [], List, Num),
	append(A, B, List).
split_(A, A, _, 1) :- !.
split_(A, A, _, 1.0) :- !.
split_(A, Acum, [H|T], Num) :-
	NumN is Num - 1,
	append(Acum, [H], Sup),
	split_(A, Sup, T, NumN).




%%%%%%%% game printing

% interactive player---interface

%% display state of game
printGame(Table, Type) :-
	players(X), userPlayer(Y),
	Total is X + Y + 1,
	write('Table:'), nl,
	printHands(Table, Total, 1, Type).

% Restrict <- Casino shows only on card
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


%% ask whether user want to grab a card
askCard(Answer) :-
	write('Do you want to draw a card? [y/n]'), nl, % [121] / [110]
	read_line_to_codes(user_input, Input),
	( Input = [121] -> Answer is 1
	; Input = [110] -> Answer is 0
	; otherwise     -> askCard(Answer)
	).
