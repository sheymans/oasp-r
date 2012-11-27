% A sample input program:

% a(X) :- f(X,Y), not b(Y).
% b(X) :- not c(X).
% c(X) :- f(X,Y), b(Y).

% c should be deduced when having b.

rule( r1, a, [  ], [ [f, n(b) ] ]).
rule( r2, b, [ n(c) ], [ ] ).
rule( r3, c, [  ], [ [f, b ] ]).


unary( a ).
unary( b ).
unary( c ).
binary( f ).

free( f ).
