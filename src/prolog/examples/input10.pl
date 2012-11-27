% A sample input program:

% a(X) :- b(X), f(X,Y), not g(X,Y).
% g(X,Y) :- b(X), c(X).

% c should be deduced when having b.

rule( r1, a, [ b ], [ [f, n(g) ]]).
rule( r2, g, [ b,c ], [] ).


unary( a ).
unary( b ).
unary( c ).

binary( f ).
binary( g ).
%binary( h ).

free( b ).
free( c ).
free( f ).
%free( h ).
