% This file describes functions to print the results.

show_result_raw( T, G ) :- 
	writeln( 'TREE:' ),
	writeln( T ),
	writeln( 'GRAPH:' ),
	writeln( G ).

produce_dot_graph( T ) :-
	% make the output stream the file TEMP
	tell('TEMP.graph'),

	% Write the tree
	
	writeln('digraph G {'),
	draw_tree( [], T ),
	writeln( ' } ' ),
	% close the output stream
	told.

draw_tree( Node, T ) :-
	draw_label( Node, T ),
	% draw "Node -> S" for each successor S, and recurse:
	forall( succT( Node, S, T ),
		(	write( '\"'), write( Node ), write( '\"'), write( ' -> ' ), write( '\"'), write( S ), write( '\"'), writeln( ' ;' ),
			draw_tree( S, T )	
		)
	).
	
draw_label([], T) :-
	ct( [], Content, T), 
	write( '\"'), write( [] ), write( '\"'), write( ' [ label=\"' ), print_list(Content), write('\"'),
	writeln(',color=red];').

draw_label( Node, T ) :-
	cached( Node, T ), caching_pair( Node, Caching, T),
	ct( Node, Content, T),
	write( '\"'), write( Node ), write( '\"'), write( ' [ label=\"' ), print_list(Content), write('\"'),
	write( ',peripheries=2' ),
	writeln('];'),
	% point to caching pair
	write( '\"'), write( Node ), write( '\"'), write( ' -> ' ), write( '\"'), write( Caching ), write( '\"'), write( '[style=dotted]'), writeln( ' ;' ).
	

draw_label( Node, T ) :-
	blocked( Node, T ), blocking_pair( Node, Blocking, T ),
	ct( Node, Content, T),
	write( '\"'), write( Node ), write( '\"'), write( ' [ label=\"' ), print_list(Content), write('\"'),
	write( ',shape=square' ),
	writeln('];'),
	% point to blocking pair
	write( '\"'), write( Node ), write( '\"'), write( ' -> ' ), write( '\"'), write( Blocking ), write( '\"'), write( '[style=dotted]'), writeln( ' ;' ).

draw_label( Node, T ) :-
	ct( Node, Content, T),
	write( '\"'), write( Node ), write( '\"'), write( ' [ label=\"' ), print_list(Content), write('\"'),
	writeln('];').  



print_list( [] ).
print_list( [A] ) :-
	pos( A ),!, % do not write negative predicates
	write(' '), write( A ).

print_list( [_] ).

print_list( [ A | Rest ] ) :-
	pos( A ),!, write(' '), write( A ), write( ',' ), print_list( Rest ).
print_list( [ _ | Rest ] ) :- 
	print_list( Rest ).


test_dot :-
	T = [	nT( [], [a,b], []),
			nT( [1], [c], []),
			nT( [2], [d,n(b)], []),
			nT( [1,1], [a,b], []),
			nT( [2,1], [a,b], []) ],
	produce_dot_graph( T, _).

