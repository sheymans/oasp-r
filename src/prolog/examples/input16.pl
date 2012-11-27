rule( r1, a, [], [ [ f, n(c)]]).
rule( r2, b, [], [ [ f, a]]).
rule( r3, c, [n(b)], []).
binary( f ).
free(f).
unary( a ).
unary( b ).
unary( c ).

%a(X):-f(X,Y), not c(Y)
%b(X):-f(X,Y),a(Y)
%c(X):-not b(X)

%it does not terminate; as example 18 shows, the problem seems to be that the repetition (blocking) is from 2 to 2 due to choosing not b in the root; if b is chosen in the root the repetition is in every single node, so it works

% FIXED: forbidden cut in ancestor relation removed (it was only checking predecessors for blocking pair candidates)
