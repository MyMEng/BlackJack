/** <module> Dealer strategies for the Blackjack game simulator

This module contains dealer strategies configuration and predicates.

@author Kacper Sokol
@license GPL
*/

:- module( dealerStrategies,
  [croupierAI/4]
  ).

:- ensure_loaded(library(aggregate)).
:- use_module(deck).

%% dealer(-Strategy) is det.
%
% Defines the strategy played by the dealer (s17 is better for the player), 
% either:
%  * `h17` - hit soft 17;
%  * `s17` - stand on ALL 17's.
%
% @param Strategy  Strategy of the dealer: `h17` or `s17` as explained above.
%
dealer(s17).

%% croupierAI(-NNTable, -NNDeck, +NTable, +NDeck) is det.
%
% Plays defined dealer strategy  based on `dealer` predicate. The dealer is
% always last in the queue to draw a new card.
%
% @param NNTable  A list of all player's cards (list of lists containing 
%                 predicates `card`) *after* dealer's decision.
% @param NNDeck   A list of cards in the deck (list of predicates `card`) 
%                 *after* dealer's decision.
% @param NTable   A list of all player's cards (list of lists containing 
%                 predicates `card`) *before* dealer's decision.
% @param NDeck    A list of cards in the deck (list of predicates `card`) 
%                 *before* dealer's decision.
%
croupierAI(NNTable, NNDeck, NTable, NDeck) :-
  dealer(X),
  ( X = s17 -> stand(NNTable, NNDeck, NTable, NDeck)
  ; X = h17 -> hit(NNTable, NNDeck, NTable, NDeck)
  ).

%% stand(-NNTable, -NNDeck, +NTable, +NDeck) is det.
%
% Plays `h17`: *hit soft 17*, strategy. The dealer is always last in the queue 
% to draw a new card.
%
% @param NNTable  A list of all player's cards (list of lists containing 
%                 predicates `card`) *after* dealer's decision.
% @param NNDeck   A list of cards in the deck (list of predicates `card`) 
%                 *after* dealer's decision.
% @param NTable   A list of all player's cards (list of lists containing 
%                 predicates `card`) *before* dealer's decision.
% @param NDeck    A list of cards in the deck (list of predicates `card`) 
%                 *before* dealer's decision.
%
stand(NNTable, NNDeck, [E|RTable], [C|NDeck]) :-
  findall( S , score( S, E ), Score ),
  aceDillemaStand(Decision, Score),
  ( Decision =< 16 -> ( append(E, [C], NewHand), NewC = [] ) % hit
  ; Decision >= 17 -> ( NewHand = E, NewC = [C] )
  ),
  append( [NewHand], RTable, NNTable ),
  append( NewC, NDeck, NNDeck ).

%% aceDillemaStand(-Decision, +Scores) is det.
%
% Decide vale of hand - can be more than one with Ace (1 or 11 points) - based 
% on current strategy and score. Decision based on **stand** strategy.
%
% @param Decision  A score of a player chose based on current strategy.
% @param Score     A list of possible score interpretation: there can be more 
%                  than one if a player has Ace in his hand.
%
aceDillemaStand(Decision, [S|Score]) :-
  aceDillemaStand(Decision, S, Score).
aceDillemaStand(Decision, S, [Sin|Score]) :-
  ( S =< 16   -> Decision = S
  ; S >= 17   -> aceDillemaStand(Decision, Sin, Score)
  ).
aceDillemaStand(S, S, []).

%% hit(-NNTable, -NNDeck, +NTable, +NDeck) is det.
%
% Plays `s17`: *stand on ALL 17's*, strategy. The dealer is always last in the  
% queue to draw a new card. If Ace & score <17 treat Ace as 11; otherwise if 
% score >17 swap value of Ace to 1.
%
% @param NNTable  A list of all player's cards (list of lists containing 
%                 predicates `card`) *after* dealer's decision.
% @param NNDeck   A list of cards in the deck (list of predicates `card`) 
%                 *after* dealer's decision.
% @param NTable   A list of all player's cards (list of lists containing 
%                 predicates `card`) *before* dealer's decision.
% @param NDeck    A list of cards in the deck (list of predicates `card`) 
%                 *before* dealer's decision.
%
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

%% aceDillemaHit(-Decision, -Type, +Aces, +Scores) is det.
%
% Decide vale and type of hand - can be more than one with Ace (1 or 11 points) 
% - based on current strategy, number of Aces in the hand and score. Decision 
% based on **hit** strategy.
%
% @param Decision  A score of a player chose based on current strategy.
% @param Type      A type of played hand.
% @param Aces      A number of aces in the hand.
% @param Score     A list of possible score interpretation: there can be more 
%                  than one if a player has Ace in his hand.
%
aceDillemaHit(Decision, Type, Aces, [S|Score]) :-
  ( Aces = 0  -> (Type = hard, Decision = S)
  ; (Aces = 1, S>17)  -> (Type = hard, nth1(1, Score, Decision)) % one ace more than 3 cards
  ; Aces = 1  -> (Type = soft, Decision = S)
  ; otherwise -> (Type = hard, aceDillemaStand(Decision, [S|Score]))
  ).
