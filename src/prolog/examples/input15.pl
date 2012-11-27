rule( r1, a, [], [ [ f, n(b)]]).
rule( r2, b, [ ], [ [ f, n(a)]]).
binary( f ).
free(f).
unary( a ).
unary( b ).

%it succeeds, but blocking happens a step too late; I suspect that it is because there is no binary predicate in the label of the root and the set inclusion test is done also over binary preds in the content of the nodes;  see also example 17

% FIXED (suspicion was correct)
