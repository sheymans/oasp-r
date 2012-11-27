% A sample input program:

% rule( Name, HeadPredicate, Beta, List of Gammas
% the rule r1: a(X) :- b(X)
rule( r1, a, [ b ], [ ]).
% the rule r2: b(X) :- not a(X).
rule( r2, b, [ n(a) ], [ ] ).
% the rule r3: b(X) :- not c(X).
rule( r3, b, [ n(c)], [ ] ).

% free predicates:
free( c ).

% classification of the predicates (could be done automatically):
unary( a ).
unary( b ).
unary( c ).
%binary( f ).
%binary( g ).
%binary( h ).
