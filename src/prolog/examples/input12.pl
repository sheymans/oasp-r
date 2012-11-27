% A sample input program:

% a(X) :- not b(X), f(X,Y).
% b(X) :- f(X,Y), not c(Y).

% c should be deduced when having b.

rule( r1, a, [ n(b)  ], [ [f] ]).
rule( r2, b, [  ], [ [f, n(c)]] ).


unary( a ).
unary( b ).
unary( c ).

binary( f ).
free( c ).
free( f ).
