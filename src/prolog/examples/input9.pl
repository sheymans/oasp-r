% A sample input program:

% c(X) :- b(X).
% a(X) :- b(X).

% c should be deduced when having b.

rule( r1, c, [ b ], [ ]).
rule( r2, a, [ b ], [ ] ).


unary( a ).
unary( b ).
unary( c ).
free( b ).
