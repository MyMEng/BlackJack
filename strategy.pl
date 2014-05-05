%% define strategy
% S17 is better for the player
%% dealer(h17). % hit soft 17
dealer(s17). % stand on ALL 17's

% play deterministic croupier strategy---H17 | S17
% dealer is always last in queue
croupierAI(NNTable, NNDeck, NTable, NDeck) :-
	dealer(X),
	( X = s17 -> stand(NNTable, NNDeck, NTable, NDeck)
	; X = h17 -> hit(NNTable, NNDeck, NTable, NDeck)
	).


stand(NNTable, NNDeck, [E|RTable], [C|NDeck]) :-
	findall( S , score( S, E ), Score ),
	aceDillemaStand(Decision, Score),
	( Decision =< 16 -> ( append(E, [C], NewHand), NewC = [] ) % hit
	; Decision >= 17 -> ( NewHand = E, NewC = [C] )
	),
	append( [NewHand], RTable, NNTable ),
	append( NewC, NDeck, NNDeck ).

aceDillemaStand(Decision, [S|Score]) :-
	aceDillemaStand(Decision, S, Score).
aceDillemaStand(Decision, S, [Sin|Score]) :-
	( S =< 16   -> Decision = S
	; S >= 17   -> aceDillemaStand(Decision, Sin, Score)
	).
aceDillemaStand(S, S, []).


% jezeli as i mniej niz 17 treat as 11 | > 17 swap for 1
hit(NNTable, NNDeck, [E|RTable], [C|NDeck]) :-
	findall( S , score( S, E ), Solutions ),
	aggregate_all(count, member(card(_,ace),E), Aces), % count number of positives
	aceDillemaHit(Score, Type, Aces, Solutions),
	( Score =< 16                  -> ( append(E, [C], NewHand), NewC = [] ) % hit
	; ( Score =  17, Type = soft ) -> ( append(E, [C], NewHand), NewC = [] ) % check for softness
	; ( Score =  17, Type = hard ) -> ( NewHand = E, NewC = [C] ) % check for hardness
	; Score >  17                  -> ( NewHand = E, NewC = [C] )
	),
	append( [NewHand], RTable, NNTable ),
	append( NewC, NDeck, NNDeck ).
	%% write(NNTable), nl, write(NNDeck), nl.

aceDillemaHit(Decision, Type, Aces, [S|Score]) :-
	( Aces = 0  -> (Type = hard, Decision = S)
	; (Aces = 1, S>17 )  -> (Type = hard, elementN(Decision, Score, 1)) % one ace more than 3 cards
	; Aces = 1  -> (Type = soft, Decision = S)
	; otherwise -> (Type = hard, aceDillemaStand(Decision, [S|Score]))
	).


% Players strategies
%% general functions

% check hand type: Soft or Hard
dealType(T, Player) :-
	aggregate_all(count, member(card(_,ace), Player), Aces), % count number of aces
	( Aces = 0  -> T = hard 
	; otherwise -> T = soft
	).

% check dealer's face-up card
dealerFaceUp( ValueR, [ _, card(_, Value)| [] ] ) :-
	( Value = jack  -> ValueR is 10
	; Value = queen -> ValueR is 10
	; Value = king  -> ValueR is 10
	; otherwise     -> ValueR = Value
	).

% attach back *Elem* to Ls at position X
attach( [Elem|Ls], Ls, Elem, 1) :- !.
attach( [Elem], [], Elem, _) :- !.
attach( NLs, [First|Ls], Elem, X) :-
	attach(NLs, [First], Ls, Elem, X, 2).

attach(NLs, Head, Tail, Elem, X, X) :-
	append(Head, [Elem|Tail] , NLs), !.
attach(NLs, Head, [Bg|Tail], Elem, X, Current) :-
	Current1 is Current + 1,
	append(Head, [Bg] , NewHead),
	attach(NLs, NewHead, Tail, Elem, X, Current1).

% replace back *Elem* to Ls at position X
replace( [Elem|Ls], [_|Ls], Elem, 1) :- !.
replace( [Elem], [], Elem, _) :- !.
replace( NLs, [First|Ls], Elem, X) :-
	replace(NLs, [First], Ls, Elem, X, 2).

replace(NLs, Head, [_|Tail], Elem, X, X) :-
	append(Head, [Elem|Tail], NLs), !.
replace(NLs, Head, [Bg|Tail], Elem, X, Current) :-
	Current1 is Current + 1,
	append(Head, [Bg] , NewHead),
	replace(NLs, NewHead, Tail, Elem, X, Current1).


%% assign strategy for each player and invoke them
playAI([Dealer|NTable1], NNDeck, NRefuse, [Dealer|NTable], NDeck, Refuse) :-
	players(X),
	playAISequence(NTable1, NNDeck, NRefuse, NTable, NDeck, Dealer, 1, X, Refuse).

playAISequence(Table, Deck, Refuse, Table, Deck, _, X1, X, Refuse) :-
	X1 is X +1,
	!.
playAISequence(NewTable, NNDeck, NRefuse, Table, NDeck, Dealer, Counter, X, Refuse) :-
	% player B is,
	elementN(PlayerB, Table, Counter),

	% do different strategy for each player
	elementN(Status, Refuse, Counter),
	( Status = 0 ->
		( Counter = 1 -> ( playAIDet( PlayerA, ChDeck, PlayerB, NDeck, Dealer ),
						   replace( NewRefuse, Refuse, 1, Counter) )
		; Counter = 2 -> ( PlayerA = PlayerB, ChDeck = NDeck, replace( NewRefuse, Refuse, 1, Counter) )
		; Counter = 3 -> ( PlayerA = PlayerB, ChDeck = NDeck, replace( NewRefuse, Refuse, 1, Counter) )
		; otherwise   -> ( playAIDet( PlayerA, ChDeck, PlayerB, NDeck, Dealer ),
						   replace( NewRefuse, Refuse, 1, Counter) )
		)
	; otherwise ->
		( PlayerA = PlayerB, ChDeck = NDeck, NewRefuse = Refuse )
	),

	% if AI once refused do not allow any more
	%% ( PlayerA = PlayerB -> replace( NewNewRefuse, NewRefuse, 1, Counter) % refusal
	%% ; otherwise         -> NewNewRefuse = NewRefuse % action taken
	%% ),
	NewNewRefuse = NewRefuse,

	% attach back player A.
	replace( ChTable, Table, PlayerA, Counter),

	Counter1 is Counter + 1,
	playAISequence(NewTable, NNDeck, NRefuse, ChTable, ChDeck, Dealer, Counter1, X, NewNewRefuse).

%% 1. Deterministic Strategy -- can only hit in the first round
playAIDet(NPlayer, NDeck, Player, Deck, Dealer) :-
	dealType(P, Player),
	dealerFaceUp(D, Dealer),
	%% write('Dealer face: '), write(D), nl,
	score(V, Player), % cards value V
	playAIDet(Action, V, P, D),
	% take an action
	( Action = hit   -> hitDet(NPlayer, NDeck, Player, Deck)
	; Action = stand -> standDet(NPlayer, NDeck, Player, Deck)
	).

playAIDet(Action, V, hard, D) :-
	hardAction(Action, D, V).
playAIDet(Action, V, soft, D) :-
	softAction(Action, D, V).

hitDet(NPlayer, Deck, Player, [C|Deck]) :-
	append(Player, [C], NPlayer).
standDet(Player, Deck, Player, Deck).


%% 2. Shuffle tracking --- Deck Probabilities
%% playAI(NTable, NDeck, NewTable, NewDeck) :- % do the AI magic
	%% i <- [1,2,3,4],
	%% <- i,
	%% I <- i,
	%% write( i(I) ), nl.

%% 1. MAB inspired - 50% contribution deterministic | 50% contribution deck memory
