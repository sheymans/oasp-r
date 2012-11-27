% This file defines some rule manipulation functions as well as tests
% on predicates.

% Get Rules with head predicate Pred

head( Name, Pred ) :-
       rule( Name, Pred, _ , _ ).

% Positive unary predicate:

pos_un( P ) :-
	unary( P ).
%	not P=n(_).

% Positive binary predicate:

pos_bin( P ) :-
	binary( P ).
	%	not P=n(_).

% Positive predicate

pos( P ) :- not P = n(_).

% Negative unary predicate:

neg_un( P ) :-
	P = n(Q),
	unary( Q ).

% Negative binary predicate:

neg_bin( P ) :-
	P=n( Q ),
	binary( Q ).

% Reverse sign of predicate: if not p, then p, if p then not p:

rev( n(P), P ) :- !.

rev( Q, n(Q) ).

% underlying predicate is free:
underlying_free( A ) :- 
	pos( A ), !, free( A ).

underlying_free( A ) :- 
	A = n( Q ), !, free( Q ).



