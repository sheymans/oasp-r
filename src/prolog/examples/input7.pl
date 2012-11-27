% A sample input program:

% a(X) :- f(X,Y), b(Y).
% f(X,Y) :- c(X), g(X,Y), not h(X,Y) not c(Y).

% a satisfiable

rule( r1, a, [  ], [ [f, b ] ]).
rule( r2, f, [ c ], [ [g, n(c), n(h) ] ]).


unary( a ).
unary( b ).
unary( c ).
binary( f ).
binary( g ).
binary( h ).

free( b ).
free( c ).
free( g ).
free( h ).
