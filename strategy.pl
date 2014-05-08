%% Define AI algorithms

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


% if: ace & <17 treat as 11 | >17 swap for 1
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
		; Counter = 2 -> ( playAIDeck( PlayerA, ChDeck, PlayerB, NDeck, Table), NewRefuse = Refuse )
		; Counter = 3 -> ( playMix( PlayerA, ChDeck, PlayerB, NDeck, Dealer, Table ),
		                   replace( NewRefuse, Refuse, 1, Counter) )
		; Counter = 4 -> ( PlayerA = PlayerB, ChDeck = NDeck, replace( NewRefuse, Refuse, 1, Counter) )
		; otherwise   -> ( playAIDet( PlayerA, ChDeck, PlayerB, NDeck, Dealer ),
						   replace( NewRefuse, Refuse, 1, Counter) )
		)
	; otherwise ->
		( PlayerA = PlayerB, ChDeck = NDeck, NewRefuse = Refuse )
	),

	% if AI once refused do not allow any more
	( ( PlayerA = PlayerB, Counter = 2) -> replace( NewNewRefuse, NewRefuse, 1, Counter) % refusal
	; otherwise                         -> NewNewRefuse = NewRefuse % action taken
	),

	% attach back player A.
	replace( ChTable, Table, PlayerA, Counter),

	Counter1 is Counter + 1,
	playAISequence(NewTable, NNDeck, NRefuse, ChTable, ChDeck, Dealer, Counter1, X, NewNewRefuse).

%% 1. Deterministic Strategy -- can only hit in the first round
playAIDet(NPlayer, NDeck, Player, Deck, Dealer) :-
	dealType(P, Player),
	dealerFaceUp(D, Dealer),
	%% write('Dealer face: '), write(D), nl,
	score(V, Player),!, % cards value V---take only first value where aces are treated as 11
	playAIDet(Action, V, P, D),
	% take an action
	( Action = hit   -> hitDet(NPlayer, NDeck, Player, Deck)
	; Action = stand -> standDet(NPlayer, NDeck, Player, Deck)
	).

playAIDet(Action, V, hard, D) :-
	hardAction(Action, D, V).
playAIDet(Action, V, soft, D) :-
	V1 is V - 11,
	softAction(Action, D, V1).

hitDet(NPlayer, Deck, Player, [C|Deck]) :-
	append(Player, [C], NPlayer).
standDet(Player, Deck, Player, Deck).


%% 2. Shuffle tracking --- Deck Probabilities --- we assume deck has started being in ordered
% get probabilities of each card occurring and decide based on the one with highest probabilitys
playAIDeck( PlayerA, ChDeck, PlayerB, [Dc|NDeck], Table) :- % do the AI magic
	%% decks(D),
	%% Size is 52 * D,
	%% deckMx <- 'Diagonal(Size, x=1)', % initialize Deck matrix
	% figure out pobabilities after shuffling given number of times
	initShuffles(Quantity),
	shuffleMode(Type),
	initDeckProbabilities(Deck, Quantity, Type),
	% count how many card have alredy been used and what is the next one
	cardsOnTable(N, Table),
	N1 is N + 1,
	elementN(Card, Deck, N1),
	append(PlayerB, [Card], TrialHand),
	scoreTop(Top, TrialHand),
	( Top =< 21 -> (append(PlayerB, [Dc], PlayerA), ChDeck = NDeck)
	; otherwise -> (PlayerB = PlayerA, append([Dc], NDeck, ChDeck))
	).

% prepare deck probability table for initial number for shuffles
initDeckProbabilities(Deck, Quantity, random) :-
	deck(DeckL), % generate deck
	initShuffles(Quantity),
	% generate x randm vectors 0/1 each corresponding to 1 shuffle
	Size is 27 * Quantity,
	Sl <- sample(0:1, Size, replace=1),
	shuffleMock(Deck, DeckL, Quantity, Sl).
initDeckProbabilities(Deck, Quantity, deterministic) :- % first from the top pile is top/././.
	deck(DeckL), % generate deck
	initShuffles(Quantity),
	findall(Shuffled, shuffleMock(Shuffled, DeckL, Quantity, []), Y),
	length(Y, Len),
	El <- sample(1:Len, 1),
	elementN(Deck, Y, El). % initial deck shuffle.


shuffleMock(Shf, Shf, 0, []) :- !.
shuffleMock(Shuffled, Deck, N, Sampling) :-
	shuffleMode(Mode),
	proper_length(Deck, Len),
	Half is Len / 2,
	getPiles(A, _, Half),
	A1 is A + 1,
	split(P1, P2, Deck, A1), % get two piles
	% random or deterministic
	( Mode = random        -> rifleRanMock(Forward, P1, P2, NSampling, Sampling)
	; Mode = deterministic -> (rifleDetMock(Forward, P1, P2), NSampling = Sampling )
	),
	N1 is N - 1,
	shuffleMock( Shuffled, Forward, N1, NSampling ).
%% rifle shuffle two piles deterministically
rifleDetMock(Out, A, B) :-
	%% random(0, 2, Rand), % decide whether left pile goes on top or bottom,
	( Rand = 0 ; Rand = 1 ),
	( Rand = 0 -> rifleDetMock(Out, [], A, B)
	; Rand = 1 -> rifleDetMock(Out, [], B, A)
	).
rifleDetMock(Out, Em, [A1|A2], [B1|B2]) :-
	append(Em, [A1], O1),
	append(O1, [B1], O2),
	rifleDetMock(Out, O2, A2, B2).
rifleDetMock(Out, Em, [], [B1|B2]) :-
	append(Em, [B1], O),
	rifleDetMock(Out, O, [], B2).
rifleDetMock(Out, Em, [A1|A2], []) :-
	append(Em, [A1], O),
	rifleDetMock(Out, O, A2, []).
rifleDetMock(Out, Out, [], []) :-
	!.

%% rifle shuffle two piles randomly
rifleRanMock(Out, A, B, NSampling, [Rand| Sampling]) :-
	%% random(0, 2, Rand), % decide whether left pile goes on top or bottom
	%% ( Rand = 0 ; Rand = 1 ),
	%% write(Rand),
	rifleRanMock(Out, [], A, B, Rand, NSampling, Sampling).
rifleRanMock(Out, Em, [A1|A2], [B1|B2], 0, NSampling, [Rand| Sampling]) :-
	append(Em, [A1], O1),
	append(O1, [B1], O2),
	%% random(0, 2, Rand),
	%% ( Rand = 0 ; Rand = 1 ),
	%% write(Rand),
	rifleRanMock(Out, O2, A2, B2, Rand, NSampling, Sampling).
rifleRanMock(Out, Em, [A1|A2], [B1|B2], 1, NSampling, [Rand| Sampling]) :-
	append(Em, [B1], O1),
	append(O1, [A1], O2),
	%% random(0, 2, Rand),
	%% ( Rand = 0 ; Rand = 1 ),
	%% write(Rand),
	rifleRanMock(Out, O2, A2, B2, Rand, NSampling, Sampling).
rifleRanMock(Out, Em, [], [B1|B2], Rand, NSampling, Sampling) :-
	append(Em, [B1], O),
	rifleRanMock(Out, O, [], B2, Rand, NSampling, Sampling).
rifleRanMock(Out, Em, [A1|A2], [], Rand, NSampling, Sampling) :-
	append(Em, [A1], O),
	rifleRanMock(Out, O, A2, [], Rand, NSampling, Sampling).
rifleRanMock(Out, Out, [], [], _, NSampling, NSampling) :-
	!.

% count number of cards on table
cardsOnTable(N, Table) :-
	cardsOnTable(N, 0, Table).
cardsOnTable(N, N, []) :- !.
cardsOnTable(N, Accum, [T|Table]) :-
	length(T, L),
	A1 is Accum + L,
	cardsOnTable(N, A1, Table).

%% 3. 50% contribution deterministic | 50% contribution deck memory
playMix( PlayerA, ChDeck, PlayerB, NDeck, Dealer, Table ) :-
	playAIDet( PlayerAa, ChDeckA, PlayerB, NDeck, Dealer ),
	playAIDeck( PlayerAb, ChDeckB, PlayerB, NDeck, Table),
	random(0, 2, Rand),
	( (PlayerAa=PlayerAb, ChDeckA=ChDeckB) -> (PlayerA=PlayerAa, ChDeck=ChDeckA)
	; Rand=0                               -> (PlayerA=PlayerAa, ChDeck=ChDeckA)
	; Rand=1                               -> (PlayerA=PlayerAb, ChDeck=ChDeckB)
	).
% create a sampling distribution over all possible cards that can be drawn and sample a card
% based on that card predict according to table whether it is worth taking it or not
