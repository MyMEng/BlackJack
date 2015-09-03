% Dealer strategies
:- module( dealerStrategies,
  [croupierAI/4]
  ).

:- ensure_loaded(library(aggregate)).
:- use_module(deck).

%% define dealer's strategy --- S17 is better for the player
%% dealer(h17). % hit soft 17
dealer(s17). % stand on ALL 17's

% play deterministic croupier strategy---H17 | S17
% dealer is always last in the queue
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
  ; (Aces = 1, S>17 )  -> (Type = hard, nth1(1, Score, Decision)) % one ace more than 3 cards
  ; Aces = 1  -> (Type = soft, Decision = S)
  ; otherwise -> (Type = hard, aceDillemaStand(Decision, [S|Score]))
  ).
