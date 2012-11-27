% This file defines the expansion and applicability rules.

%% The update function from the paper (beginning of section on Expansion rules).
% L is a node in the dependency graph (i.e., it represents an atom)
% P is a (possibly negated) pred
% Z is the node where p has to be inserted
update( L, P, Z, Tree, graph( Nodes, Edges), NewTree, graph( NewNodes, NewEdges ) ) :-
	update_content_status( P, Z, Tree, NewTree ),
	update_nodesG( P, Z, Nodes, NewNodes),
	update_edgesG( L, P, Z, Edges, NewEdges),!.

% update content and status for a negative unary predicate, if the predicate is not yet in the content of Z.
update_content_status( n(P), Z, Tree, Tree ) :-
	unary( P ),
	% n(P) is already in the content, so don't do anything
	in_ct( n(P), Z, Tree),!.

update_content_status( n(P), Z, Tree, NewTree ) :-
	unary( P ), !,
	add_to_content( Z, n(P), Tree, TempTree ),
	% add the status unexpanded for all rules with head predicate Pred:
	change_all_st_negunary( Z, n(P), 'nexp', TempTree, NewTree ).

% update content and status for a positive (unary or binary) predicate or a negative binary predicate, if the the predicate is not yet in the content of Z.
update_content_status( P, Z, Tree, Tree ) :-
	% P is already in the content, so don't do anything
	in_ct( P, Z, Tree), !.

update_content_status( P, Z, Tree, NewTree ) :- !,
	add_to_content( Z, P, Tree, TempTree ),
	change_st( Z, P, 'nexp', TempTree, NewTree ).

% if P is positive and not already in nodesG, then add it.
update_nodesG( P, Z, Nodes, NewNodes ) :- !,
	% P does not unify with something negated and is not in the graph
	( not ( P = n(_) ), not get_node( uatom( P, Z ), graph( Nodes, _ )) )
	-> % then
	addnode_to_graph( P, Z, graph( Nodes, _ ), graph( NewNodes, _) )
	; % Else
	NewNodes = Nodes.

update_edgesG( L, P, Z, Edges, NewEdges ) :-
	( not ( P = n(_) ), not ( L = n(_) ) )
	->
	( addedge_to_graph( L, uatom( P, Z ), graph(_,Edges), graph(_,NewEdges) ) )
	;
	NewEdges = Edges.
	
% The variant of 'update/3' with a list of Predicates Ps instead of one P:
update_list( _, [], _, Tree, Graph, Tree, Graph ).

update_list( L, [P|Ps], Z, Tree, Graph, NewTree, NewGraph ) :-
	update( L, P, Z, Tree, Graph, TempTree, TempGraph ),
	update_list( L, Ps, Z, TempTree, TempGraph, NewTree, NewGraph),
	!.


%%%%% Expand Unary Positive Rule

%% Condition for expand unary positive rule on the predicate P in node
%X:
ex_un_pos( P, X, Tree ) :-
	% P is in the content of X:
	in_ct( P, X, Tree ),	
	% P is a positive unary predicate:
	pos_un( P ),
	% P is non-free: (removed this, if it is free the expansion
	% rule should put its status on expanded)
	%not free( P ),
	% status is not expanded:
	st( X, P, 'nexp', Tree ).

apply_expand_unary_pos( P, X, Tree, Graph, NewTree, Graph ) :-
	free( P ),!, % if it is free just put its status on expanded
	change_st( X, P, 'exp', Tree, NewTree ).

apply_expand_unary_pos( P, X, Tree, Graph, NewTree, NewGraph ) :-
	% nondeterministically choose a rule with with head P:
	rule( _, P, Beta, Gammas),
	% update beta
	update_list( uatom( P, X ), Beta, X, Tree, Graph, NewT1, NewG1 ),
	% for each gamma, pick a successor Y or create a new successor
	% Y
	% of X and update it according to gamma:
	pick_and_update( Gammas, P, X, NewT1, NewG1, NewT2, NewGraph),  
	% set status expanded:
	change_st( X, P, 'exp', NewT2, NewTree ).

% pick a successor of X and update it with the gammas, connecting in
% the graph the successors with p(x)

pick_and_update( [], _, _, Tree, Graph, Tree, Graph).

pick_and_update( [ Gamma | Gammas ], P, X, Tree, Graph, NewTree, NewGraph) :- 
	get_successor( X, Y, Tree, TempTree ),
	update_list( uatom( P, X ), Gamma, Y, TempTree, Graph, NewT, NewG),
	pick_and_update( Gammas, P, X, NewT, NewG, NewTree, NewGraph
	).



	
%%%%% Expand Unary Negative Rule

%% Condition for expand unary negative rule on the predicate P in node
%X:
ex_un_neg( P, X, RuleName, Tree ) :-
	% P is in the content of X:
	in_ct( P, X, Tree ),	
	% P is a unary negative predicate:
	neg_un( P ),
	P = n( Q ),
	% Q is non-free: % this condition should not be here I think (even if it is free, it should be expanded)
	%	not free( Q ),
	
	head( RuleName, Q ),
	st_negunary( X, P, RuleName, 'nexp', Tree).

apply_expand_unary_neg( P, X, RuleName, Tree, Graph, NewTree, NewGraph ) :-
	% Option 1:
	% nondeterministically choose a (possibly negative) predicate
	% q in Beta:
	( expan_unneg_optiona( P, X, RuleName, Tree, Graph, Tree1,
	NewGraph )
	;
	% Option 2 refute one of the Gammas
	expan_unneg_optionb( P, X, RuleName, Tree, Graph, Tree1, NewGraph )
	),
	% set status expanded:
	change_st_negunary( X, P, RuleName, 'exp', Tree1, NewTree ).

expan_unneg_optiona( P, X, RuleName, Tree, Graph, NewTree, NewGraph ) :- 
	rule( RuleName, _, Beta, _ ),
	% Not that we need to use member here instead of membcheck, to
	% bind Q (membchk uses == to compare elements so does not
	% bind)
	member( Q, Beta ),
	% reverse the sign of Q and update:
	rev( Q, R ),
	update( n( uatom(P, X) ), R, X, Tree, Graph, NewTree, NewGraph
	).  
	
expan_unneg_optionb( P, X, RuleName, Tree, Graph, NewTree, NewGraph ) :- 
	%	write( 'TRYING Option b with: '),
	rule( RuleName, _, _,  Gammas ),
	% pick a Gamma in Gammas
	member( Gamma, Gammas ),
	% for each successor of X, pick something in Gamma to refute
	findall( Y, succT(X,Y, Tree), Succs),
	pick_gamma_update( Gamma, Succs, X, P, Tree, Graph, NewTree, NewGraph ).

pick_gamma_update( _, [], _, _, Tree, Graph, Tree, Graph).


pick_gamma_update( Gamma, [ S | Succs], X, P, Tree, Graph, NewTree, NewGraph ) :-
	member( Q, Gamma ),
	rev( Q, R ),
	update( n( uatom( P, X ) ), R, S, Tree, Graph, Tree1, Graph1 ),
	pick_gamma_update( Gamma, Succs, X, P, Tree1, Graph1, NewTree, NewGraph ).

	
%%%%% Choose a unary predicate

% all positive nodes in X are expanded and there is a predicate that
% is not (or its negation) yet in the content:
choose_un( X, P, Tree ) :-
	% First bind X, to use it in the forall, X is *some* node from
	% the tree:
	ct( X, _, Tree),
	% we leave the next test out, as it can be taken care of in the order the expand rules are called (namely choose unary should be called after the others)
	forall( ( in_ct( A, X, Tree ), not neg_un( A ) )  , ( st( X, A, 'exp', Tree ) ; underlying_free( A ) ) ), 
	% A could be negative, so we have to check that the underlying
	% predicate is free (negative predicates do not have an
	% expanded status like positive ones)
	unary( P ), 
	not in_ct( P, X, Tree ), not in_ct( n(P), X, Tree ).

% apply the choose unary predicate rule to the node X for predicate
% (possibly negated) Q:
apply_choose_unary( X, P, Tree, NewTree ) :-
	% choose to add P or not P:
	( Q = n(P) ; Q = P ),
	( Q = n( _ ) -> change_all_st_negunary( X, Q, 'nexp', Tree, Tree1 ) 
	;
	change_st( X, Q, 'nexp', Tree, Tree1) ),
	% add the choice to the content:
	add_to_content( X, Q, Tree1, NewTree ).

%%%%% Expand binary positive

ex_bin_pos( P, X, Tree ) :-
	% P is in the content of X:
	in_ct( P, X, Tree ),	
	% P is a positive unary predicate:
	pos_bin( P ),
	% P is non-free:
	not free( P ),
	% status is not expanded:
	st( X, P, 'nexp', Tree ).

apply_expand_binary_pos( P, X, Tree, Graph, NewTree, NewGraph ) :-
	% nondeterministically choose a rule with with head P: f(X,Y) :- Beta(X), Gamma(X,Y), Delta(Y)
	rule( _, P, Beta, Gammas),
	% in a binary rule, Gammas contains only one Gamma
	member( Gamma, Gammas ),
	% update beta in predecessor
	predT( X, Y, Tree ),
	update_list( uatom( P, X ), Beta, Y, Tree, Graph, NewT1, NewG1 ),
	update_list( uatom( P, X ), Gamma, X, NewT1, NewG1, NewT2, NewGraph ),
	% set status expanded:
	change_st( X, P, 'exp', NewT2, NewTree ).

%%%%% Expand Binary Negative Rule

ex_bin_neg( P, X, Tree ) :-
	% P is in the content of X:
	in_ct( P, X, Tree ),	
	% P is a negative binary predicate:
	neg_bin( P ),
	% left out freeness condition compared to paper
	% status is not expanded:
	st( X, P, 'nexp', Tree ).

apply_expand_binary_neg( P, X, Tree, Graph, NewTree, NewGraph ) :-
	P = n( Q ),
	% collect all rules with head Q (they should all be refuted)
	findall( RuleName, head( RuleName, Q), RuleNames ),
	refute_all( RuleNames, P, X, Tree, Graph, NewT1, NewGraph ),
	change_st( X, P, 'exp', NewT1, NewTree ).

refute_all( [], _, _, Tree, Graph, Tree, Graph ).
refute_all( [Rule | RuleNames], P, X, Tree, Graph, NewTree, NewGraph ) :-
	rule( Rule, _, Beta, Gammas),
	% choose something in beta to refute or choose something in gamma to refute:
	((
		member( Q, Beta ),
		% Y is the predecessor of X
		predT( X, Y, Tree ),
		rev( Q, QRev ),
		update( n(uatom( P, X )), QRev, Y, Tree, Graph, NewT1, NewG1 )
	)
	;
	(
		member( Gamma, Gammas ),
		member( F, Gamma ),
		rev( F, FRev ),
		update( n(uatom( P, X )), FRev, X, Tree, Graph, NewT1, NewG1 )
	)),
	refute_all( RuleNames, P, X, NewT1, NewG1, NewTree, NewGraph ).


%%%%% Choose a binary predicate

% all positive nodes in X are expanded and there is a predicate that
% is not (or its negation) yet in the content:
choose_bin( X, P, Tree ) :-
	% First bind X, to use it in the forall, X is *some* node from
	% the tree:
	ct( X, _, Tree),
	not X = [], % do not add binary predicates to the root
	binary( P ), 
	not in_ct( P, X, Tree ), not in_ct( n(P), X, Tree ).

% apply the choose unary predicate rule to the node X for predicate
% (possibly negated) Q:
apply_choose_binary( X, P, Tree, NewTree ) :-
	% choose to add P or not P:
	( Q = n(P) ; Q = P ),
	change_st( X, Q, 'nexp', Tree, Tree1),
	% add the choice to the content:
	add_to_content( X, Q, Tree1, NewTree ).


%%%% Applicability Rules


%%% Saturation:

saturated( X, Tree ) :-
	ct( X, _, Tree ), % binding X as a node in the tree
	forall( unary( P ), ( in_ct( P, X, Tree ) ; in_ct( n( P ), X, Tree ) )),
	( in_ct( A, X, Tree ) -> 
				( not (	ex_un_pos( A, X, Tree ) ;
						ex_un_neg( A, X, _, Tree )
					)
				)
				;
				true
			),
	forall( (binary( P ), succT( X, Y, Tree )) , ( in_ct( P, Y, Tree ) ; in_ct( n( P ), Y, Tree ))),
	( in_ct( F, Y, Tree ) -> 
				( not (	ex_bin_pos( F, X, Tree ) ;
						ex_bin_neg( F, X, Tree )
						)
				)
				;
				true
			).

blocking_pair( X, Z, Tree ) :-
	not X = [], % x is not the root
	% its predecessor is saturated
	predT( X, Y, Tree ),
	saturated( Y, Tree ),
	% there is an ancestor with subset content
	ancestorT( X, Z, Tree ),
	subset_ct( X, Z, Tree ).

blocked( X, Tree ) :-
	blocking_pair( X, _, Tree ).

caching_pair( X, Z, Tree ) :-
	not X = [], % x is not the root
	% its predecessor is saturated
	predT( X, Y, Tree ),
	saturated( Y, Tree ),
	% pick a node in Tree
	ct( Z, _, Tree ), not Z = X,
	% Y is not an ancestor
	not ancestorT( X, Z, Tree ),
	subset_ct( X, Z, Tree ).

cached( X, Tree ) :- 
	caching_pair( X, Y, Tree ),
	% Y itself cannot be cached
	not cached( Y, Tree ).

applicable( [], _ ).

applicable( X, T ) :-
	predT( X, Y, T ),
	saturated( Y, T ),
	not blocked( X, T ),
	not cached( X, T ).


%%%% The expansion function.

% -. take the unary positive rule for some predicate and some node:
expand( Tree, Graph, ExpTree, ExpGraph ) :- 
	ex_un_pos( P, X, Tree ),
	applicable( X, Tree ),
	!, % THIS CUT IS IMPORTANT (once a
	% rule is applicable it shoul be applied! How it is applied is
	% nondeterministic. If this cut would not be here, every expand
	% would succeed.
	apply_expand_unary_pos( P, X, Tree, Graph, NewTree, NewGraph ),
	% and expand further
	expand( NewTree, NewGraph, ExpTree, ExpGraph ).

% -. take the unary negative rule for some predicate, some node, and
% some rule:
expand( Tree, Graph, ExpTree, ExpGraph ) :- 
	ex_un_neg( P, X, Rule, Tree ), 
	applicable( X, Tree ),!,
	apply_expand_unary_neg( P, X, Rule, Tree, Graph, NewTree, NewGraph ),
	% and expand further
	expand( NewTree, NewGraph, ExpTree, ExpGraph ).


% -. take the binary positive rule for some predicate and some node:
expand( Tree, Graph, ExpTree, ExpGraph ) :- 
	ex_bin_pos( P, X, Tree ),!, %Note that we do not put the applicability rule for binary predicates in nodes, as this is assumed to be a predicate in an edge (and saturated is not defined for edges).
	apply_expand_binary_pos( P, X, Tree, Graph, NewTree, NewGraph ),
	% and expand further
	expand( NewTree, NewGraph, ExpTree, ExpGraph ).

% -. take the binary negative rule for some predicate, some node, and
% some rule:
expand( Tree, Graph, ExpTree, ExpGraph ) :- 
	ex_bin_neg( P, X, Tree ),!,
	apply_expand_binary_neg( P, X, Tree, Graph, NewTree, NewGraph ),
	% and expand further
	expand( NewTree, NewGraph, ExpTree, ExpGraph ).


% -. take choose unary rule 
expand( Tree, Graph, ExpTree, ExpGraph) :- 
	choose_un( X, Q, Tree ), applicable( X, Tree ),!,
	apply_choose_unary( X, Q, Tree, NewTree ),
	expand( NewTree, Graph, ExpTree, ExpGraph ).

% -. take choose binary rule 
expand( Tree, Graph, ExpTree, ExpGraph) :- 
	choose_bin( X, Q, Tree ), !,
	apply_choose_binary( X, Q, Tree, NewTree ),
	expand( NewTree, Graph, ExpTree, ExpGraph ).

expand( Tree, Graph, Tree, Graph).
