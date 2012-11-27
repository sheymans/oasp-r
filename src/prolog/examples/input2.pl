% A sample input program:

% a(X) :- not b(X).
% b(X) :- not a (X).

% Both a and b should be satisfiable

rule( r1, a, [ n(b) ], [ ]).
rule( r2, b, [ n(a) ], [ ] ).


unary( a ).
unary( b ).
