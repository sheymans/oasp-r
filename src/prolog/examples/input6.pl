% A sample input program:

% a(X) :- f(X,Y), b(Y), g(X,Z), not b(Z).

% a satisfiable (should introduce 2 successors)

rule( r1, a, [  ], [ [f, b ], [ g, n(b)] ]).


unary( a ).
unary( b ).
binary( f ).
binary( g ).

free( f ).
free( b ).
free( g ).
