rule( r1, a, [n(b)], [ [ f, n(g)], [g, n(f)]]).
rule( r2, b, [ ], [ [ f], [ g]]).
binary( f ).
binary( g ).
free(f).
free(g).
unary( a ).
unary( b ).

%just checking the unsatisfiability of a negative pred when it can be satisfied using things from two different successors
%it works!