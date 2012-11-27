% This file describes functions to test whether a program is a simple CoLP.

% for the dependency graph
:- dynamic dep/2, marked/2.

% Simple CoLP test as in http://www.kr.tuwien.ac.at/staff/heymans/priv/publications/alpsws2008.pdf (Def 1).

simple_colp( File ) :- 
	cl( File ),
	% Note that the syntax we use in this implementation is already restricting the type of programs, remains to check:
	% Does every list of binary items contains at least one positive one:
	( 
	%% IF
	forall( ( 
				rule( _, _, _, BinaryList ),
				member( Binaries, BinaryList)
			),
			(
				% There exists a positive binary predicate
				member( Pred, Binaries ),
				pos_bin( Pred )
			)
		)
		-> % Then
		true
		; % ELSE
		throw( 'The input program is not a simple CoLP: positive binary predicates missing. Processing aborted.' )
		),
		
		% there cannot be a marked cycle in the dependency graph:
		dependency_graph,
		( marked_cycle -> throw( 'The input program is not a simple CoLP: the marked dependency graph contains cycles. Processing aborted.' ) ; true ).
	%	not marked_cycle,
		% clean up dependency graph
%		retractall( dep( _, _ ) ), retractall( marked( _, _ ) ).



dependency_graph :- 
	forall(		rule(_,Head,Beta, Gammas),
					(
					forall( 
						( member( B, Beta ), pos( B ) ),
						assert( dep( Head, B ) )
						),
					forall( 
						( member( Gamma, Gammas ), member( F, Gamma ), pos( F ) ),
						( unary( F ) -> ( assert( dep( Head, F ) ), assert( marked( Head, F )) ) ; assert( dep( Head, F ) ) )											
						)
					)
			).

% a predicate A is connected to an another predicate B in the dependency graph via a marked edge.

reach_dep( A, B ) :- dep( A, B ), marked( A, B ),!.
reach_dep( A, B ) :- dep( A, C ), reach_dep( C, B ).

marked_cycle :- reach_dep( X, X ).





