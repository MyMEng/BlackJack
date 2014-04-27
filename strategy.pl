%% define strategy
% S17 is better for the player
dealer(h17). % hit soft 17
%% dealer(s17). % stand on ALL 17's

% play deterministic croupier strategy---H17 | S17
% dealer is always 1 in queue
croupierAI(NNTable, NNDeck, NTable, NDeck) :-
	dealer(X),
	( X = s17 -> stand(NNTable, NNDeck, NTable, NDeck)
	; X = h17 -> hit(NNTable, NNDeck, NTable, NDeck)
	).

stand(NNTable, NNDeck, [E|RTable], [C|NDeck]) :-
	score( Score, E ),
	( Score =< 16 -> ( append(E, [C], NewHand), NewC = [] ) % hit
	; Score => 17 -> ( NewHand = E, NewC = C )
	),
	append( NewHand, RTable, NNTable ),
	append( NewC, NDeck, NNDeck ).

% jezeli as i mniej niz 17 treat as 11 | > 17 swap for 1
% zr
hit(NNTable, NNDeck, [E|RTable], [C|NDeck]) :-
	findall( Score , score( Score, E ), Solutions ),
	( Score =< 16 -> ( append(E, [C], NewHand), NewC = [] ) % hit
	; Score =  17 -> % check for softness
	; Score >  17 -> ( NewHand = E, NewC = C )
	),
	append( NewHand, RTable, NNTable ),
	append( NewC, NDeck, NNDeck ).

%% players comunicate | do not comunicate--- team/oponents

%% same strategy different strategy

%% etc.


%% playAI(NTable, NDeck, NewTable, NewDeck), % do the AI magic

