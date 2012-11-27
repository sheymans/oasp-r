% A sample input program:

% a(X) :- not a(X).
% a(X) :- b(X).

% a satisfiable

rule( r1, a, [ n(a) ], [ ]).
rule( r2, a, [ b ], [ ]).


unary( a ).
unary( b ).
free( b ).
