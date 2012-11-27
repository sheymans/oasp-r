% A sample input program:

% a(X) :- b(X), not c(X).
% b(X) :- a(X).

% a is not satisfiable

rule( r1, a, [ b, n(c) ], [ ]).
rule( r2, b, [ a ], [ ]).


unary( a ).
unary( b ).
unary( c ).
free( c ).
