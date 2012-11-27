%% This file defines the clashing conditions.

% A tree T and dependency graph G are clash-free if the tree is not contradictory and the graph does not contain cycles.
clash_free( T, G ) :- 
	not contradictory( T ),
	not cycle( G ).

% A tree is contradictory if there is a node X that contains a predicate and its negation.
contradictory( T ) :- 
	in_ct( A, X, T ),
	in_ct( n( A ), X, T).

% Check for cycles in the dependency graph.
cycle( G ) :- connect( X , X, G ).

connect( X, Y, G ) :- get_edge( X, Y, G ).
connect( X, Y, G) :- get_edge( X, Z, G ), connect( Z, Y, G ).
