% This file defines the completion structure and operations on this structure.


:- dynamic free/1.


% A tree T is a list with the following elements nT( [], [a,b,n(c)],
% [sT(a,exp), sT(b, nexp), sT(n(c),r,exp)] ) where the second argument
% is the content of the first argument (the node, ie a list)

% The graph G that keeps the dependencies between the atoms is a pair
% graph( Nodes, Edges ) where Nodes is a list of nodes uatom( a, NodeT
% ) where NodeT is a node in T (encoded as a list). Edges is a set of edgeG( Atom1, Atom2 )
% Note that for we also denoted f(x,y) with uatom(f, y) where we have that the predecessor of x is unique as T is a tree, and we can thus assume it is implicit. 

% Pick content of a node in a tree T
ct( Node, Content, T ) :-
	member( nT( Node, Content, _ ), T ).

% Something is in the content of a node in a tree T:
in_ct( P, X, T ) :- 
	ct( X, Content, T), 
	member( P, Content ).

% Two nodes have contents that are subsets (only considering unary
% predicates) - note that binary predicates should not be taking into account, so we first remove the binary predicates from the ct.
subset_ct( X, Y, T ) :-
	ct( X, C1, T ),
	ct( Y, C2, T ),
	subset_unary( C1, C2 ).

subset_unary( [], _ ).
subset_unary( [ P | C1 ], C2 ) :-
	( ( pos_un( P ) ; neg_un( P ) ) -> member( P, C2 ) ; true ),
	subset_unary( C1, C2 ).

% Add a pred or a negated predicate to the content of a node
add_to_content( Node, Pred, T, T ) :-
	% if it is already in there, do not add it.
	in_ct( Pred, Node, T), !.

add_to_content( Node, Pred, T, ResultTree ) :-
	member( nT( Node, Content, Statuses ), T ),!,
	( ( rev( Pred, R ), in_ct( R, Node, T ) ) -> fail % if the opposite is already in the content then fail.
	;
	(	delete( T, nT( Node, Content, Statuses), TNew),
		append( [ nT( Node, [ Pred | Content ], Statuses) ], TNew, ResultTree )
	)).

% get the node in a graph:
get_node( X, graph( Nodes, _) ) :- member( nodeG( X ), Nodes).

% get the edge in a graph:
get_edge( X, Y, graph( _, Edges) ) :- member( edgeG( X, Y ), Edges).

% add a unary atom with predicate Pred and argument Arg to dependency graph (with the above remark Pred could also be binary)
addnode_to_graph( Pred, Arg, graph( NodesG, EdgesG ), graph( NodesResult, EdgesG) ) :- 
	% treat it like a set such that no duplicates are introduced:
	union( [ nodeG( uatom( Pred, Arg ) ) ], NodesG, NodesResult ).

addedge_to_graph( Node1, Node2, graph( NodesG, EdgesG ), graph( NodesG, EdgesResult) ) :-
	union( [ edgeG( Node1, Node2 ) ], EdgesG, EdgesResult ).


% get status E of a Node, Pred
st( Node, Pred, E, T ) :-
	member( nT( Node, _, Statuses ), T ),
	member( sT( Pred, E ), Statuses ).

% get status E of a Node, unary negative Pred, and a Rule
st_negunary( Node, Pred, Rule, E, T ) :-
	member( nT( Node, _, Statuses ), T ),
	member( sT_negunary( Pred, Rule, E ), Statuses ).

% Update status of a node Node and a Predicate Pred to a state E. This
% assumes there is already a status (it is thus my responsibility to
% add statuses before changing them)
change_st( Node, Pred, E, T, ResultTree ) :- !,
	delete( T, nT( Node, Content, Statuses), TNew),
	delete( Statuses, sT( Pred, _ ), ST ),
	append( [ sT( Pred, E ) ], ST, NewStatuses),
	append( [ nT( Node, Content, NewStatuses ) ], TNew,
	ResultTree).
	
% Update status of a node Node and a negative unary Predicate Pred and
% a Rule to a state E. This assumes there is already a status.
change_st_negunary( Node, Pred, Rule, E, T, ResultTree ) :- !,
	delete( T, nT( Node, Content, Statuses), TNew),
	delete( Statuses, sT_negunary( Pred, Rule, _ ), ST ),
	append( [ sT_negunary( Pred, Rule, E ) ], ST, NewStatuses),
	append( [ nT( Node, Content, NewStatuses ) ], TNew, ResultTree).

% Update the status for a Node and a negative unary Predicate for all
% the rules with the predicate as a head:
change_all_st_negunary( Node, n(P), E, T, ResultTree ) :- !,
	findall( Name, head( Name, P ), RuleNames ),
	% accumulator
	change_all_st_negunary_acc( RuleNames, Node, n(P), E, T, ResultTree ).

change_all_st_negunary_acc( [], _, n(_), _, T, T ) :- !.
change_all_st_negunary_acc( [ Name | RuleNames], Node, n(P), E, T, ResultTree ) :-
	change_st_negunary( Node, n(P), Name, E, T, TempTree ),
	change_all_st_negunary_acc( RuleNames, Node, n(P), E, TempTree, ResultTree ).


% Create the initial structure for a Pred:
initial_structure( Pred, InitialTree, InitialGraph ) :- !,
	% has a root [], with content [Pred], and status for Pred is
	% nexp:
	InitialTree = [ nT( [], [Pred], [ sT( Pred, 'nexp' ) ] ) ],
	% Nodes are Pred([]) initially and no edges:
	InitialGraph = graph( [ nodeG( uatom( Pred, [] ) ) ] , [] ). 



%%% Manipulate the tree (get successors, make new successors).

% A tree has a root [], its 3 successors are e.g. [1], [2], [3]. The
% 2 successors of [1] are [1,1] and [2,1], i.e., one starts from the
% back to find the path from the root to the node.

% Y  is an existing successor of X
succT( X, Y, T  ) :- 
	Y = [ _ | X ],
	ct( Y,_, T).

% Get biggest successor Y of X (right most successor):
max_succT( X, [A | X], T ) :- 
	succT( X, [A | X], T ),
	not ( succT( X, [B | X], T ), B > A ).

% Create a new successor Y of X:
new_succT( X, [A | X], T, TNew ) :- 
	( max_succT( X, [M | X ], T ) -> A is M + 1 ; A is 1 ),
	% add a node to T:
	append( [ nT([A|X],[],[])], T, TNew ).

% get either an existing successor or make a new one (should there be
% a bound on the amount of successors here, to make the algorithm
% finish? Otherwise new successors will be introduced all the time in
% case of failure) (TNew will contain the new successor).
get_successor( X, Y, T, T ) :- 
	succT( X, Y, T ).
	
get_successor( X, Y, T, TNew ) :- 
	new_succT( X, Y, T, TNew ).

% Y is the predecessor of X:
predT( X, Y, _ ) :-
	X = [ _ | Y ].
%ancestor(X,Y).
ancestorT( X, Y, _ ) :- predT( X, Y, _ ).
ancestorT( X, Y, _) :- predT( X, Z, _ ), ancestorT( Z, Y, _).

