rule( r1, a, [n(c)], [ [ f, n(g), c,b]]).
rule( r2, c, [ ], [ [ g, b]]).
binary(f).
binary(g).
free(f).
free(g).
free(b).
unary( a ).
unary( b ).
unary( c ).

%tries to capture the same problem as 15 but here the set inclusion w.r.t. unary holds and the set inclusion w.r.t. binary fails because of the presence of g in one node and not g in the other, and not because of the absence of binaries in the root - this should still be a blocking condition; so blocking happens again a step too late

% works now I think.
