% A sample input program:

% a(X) :- f(X,Y), b(Y), g(X,Z), c(Z).

% a satisfiable

rule( r1, a, [  ], [ [f, b ], [ g, c] ]).


unary( a ).
unary( b ).
unary( c ).
binary( f ).
binary( g ).

free( f ).
free( b ).
free( c ).
free( g ).
