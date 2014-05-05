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
dealerFaceUp( Value, [ _, card(_, Value)| [] ] ).

%% assign strategy for each player and invoke them
playAI(_, _, _, _).

%% 1. Deterministic Strategy -- can only hit in the first round
playAIDet(NPlayer, NDeck, Player, Deck, Dealer) :-
	dealType(P, Player),
	dealerFaceUp(D, Dealer),
	score(V, Player), % cards value V
	playAIDet(Action, V, P, D),
	% take an action
	( Action = hit   -> T = hard 
	; Action = stand -> T = soft
	).

playAIDet(Action, V, hard, D) :-
	hardAction(Action, D, V).
playAIDet(Action, V, soft, D) :-
	softAction(Action, D, V).


%% 2. Shuffle tracking --- Deck Probabilities
%% playAI(NTable, NDeck, NewTable, NewDeck) :- % do the AI magic
	%% i <- [1,2,3,4],
	%% <- i,
	%% I <- i,
	%% write( i(I) ), nl.

%% 1. MAB inspired
