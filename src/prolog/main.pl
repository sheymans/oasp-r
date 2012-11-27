% load the completion structure info.
:- cl( 'compl_struc' ).
% load rule manipulation
:- cl('rule_manip').
% load the check for simple CoLPs
:- cl('simple_colp').
% load the expansion and applicability rules.
:- cl( 'expand' ).
% load the clash-freeness tests
:- cl( 'clash' ).
% load the printing of the result.
:- cl( 'show_result' ).


% unary, binary, and free need to be dynamic, as it could be that certain input is not defining them:
:- dynamic free/1, binary/1, unary/1.

initial_setup( Pred, File, Tree, Graph ) :- 
	cl( File), initial_structure( Pred, Tree, Graph ),!.

% 'sat/2' loads the input program File and the predicate Pred to check for satisfiability:

sat_raw( Pred, File ) :-	
	initial_setup( Pred, File, Tree, Graph ),
	expand( Tree, Graph, NewTree, NewGraph ),
	clash_free( NewTree, NewGraph ),!, show_result_raw( NewTree, NewGraph).

% Produce a Graphivz dot file.
sat_dot( Pred, File ) :-	
	initial_setup( Pred, File, Tree, Graph ),
	expand( Tree, Graph, NewTree, NewGraph ),
	clash_free( NewTree, NewGraph ),!, produce_dot_graph( NewTree ).


% Satisfiability test, that also checks for syntax of the input program, in this case whether it is a simple CoLP or not.
sat_simple_colp_raw( Pred, File ) :-
	simple_colp( File ),
	initial_setup( Pred, File, Tree, Graph ),
	expand( Tree, Graph, NewTree, NewGraph ),
	clash_free( NewTree, NewGraph ),!, show_result_raw( NewTree, NewGraph ).

sat_simple_colp_dot( Pred, File ) :-
	simple_colp( File ),
	initial_setup( Pred, File, Tree, Graph ),
	expand( Tree, Graph, NewTree, NewGraph ),
	clash_free( NewTree, NewGraph ),!, produce_dot_graph( NewTree ).


%% without loading of input files.
sat( Pred ) :- 
	initial_structure( Pred, Tree, Graph ), !,
	expand( Tree, Graph, NewTree, NewGraph ),
	clash_free( NewTree, NewGraph ). 

sat_dot( Pred ) :- 
	initial_structure( Pred, Tree, Graph ), !,
	expand( Tree, Graph, NewTree, NewGraph ),
	clash_free( NewTree, NewGraph ),!,produce_dot_graph( NewTree ).


