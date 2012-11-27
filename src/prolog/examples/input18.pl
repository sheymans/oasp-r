rule( r1, a, [b], [ [ f, n(c)]]).
rule( r2, b, [], [ [ f, a]]).
rule( r2, c, [n(b)], []).
binary( f ).
free(f).
unary( a ).
unary( b ).
unary( c ).

%a(X):-b(X), f(X,Y), not c(Y)
%b(X):-f(X,Y),a(Y)
%c(X):-not b(X)
%it is not a CoLP, but it shows the problem with input 16; it does terminate when b is forced to be in the root

% Works also now, but can you check whether this nonterminating problem can still occur?
