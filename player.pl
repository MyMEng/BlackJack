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
