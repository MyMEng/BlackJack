% interactive player---interface

%% display state of game
printGame(Table) :-
	players(X), userPlayer(Y),
	Total is X + Y + 1,
	write('Table:'), nl,
	printHands(Table, Total, 1).
printHands([H|T], Total, N) :-
	userPlayer(U),
	( N = 1                -> write('Casino')
	; ( N = Total, U = 1 ) -> write('My')
	; otherwise            -> ( write('AI'), No is N - 1, write(No) )
	),
	write(':	'),
	printHand(H), nl,
	N =< Total,
	N1 is N + 1,
	printHands(T, Total, N1).
printHands([], N, N1) :-
	N1 is N + 1.
printHands([], _).
printHand([card( Suit, Rank )|T]) :-
	write(Suit),
	( Rank = ace    -> write('A')
	; Rank = jack   -> write('J')
	; Rank = queen  -> write('Q')
	; Rank = king   -> write('K')
	; integer(Rank) -> write(Rank)
	),
	write(' '),
	printHand(T).
printHand([]).


%% ask whether user want to grab a card
askCard(Answer) :-
	write('Do you want to draw a card? [y/n]'), nl, % [121] / [110]
	read_line_to_codes(user_input, Input),
	( Input = [121] -> Answer is 1
	; Input = [110] -> Answer is 0
	; otherwise     -> askCard(Answer)
	).
