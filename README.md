# `oasp-r`: A Reasoner for Simple Conceptual Programs under the Open Answer Set Semantics


## General information

The theory behind this reasoner has been written down in a [RR 2009](http://stijnheymans.net/pubs/rr2009.pdf) paper.

## Dependencies

The reasoner is written in Prolog, in particular BProlog. The code has been
tested with _B-Prolog Version 7.8, All rights reserved, (C) Afany Software
1994-2012_, namely the version for Mac Os X (`Darwin Kernel Version 11.4.0: Mon
Apr  9 19:32:15 PDT 2012; root:xnu-1699.26.8~1/RELEASE_X86_64`). If you have
issues with newer/older version of BProlog, please drop me a line.

## Installation and Runnng the Code

- Go to the directory containing the file `main.pl` (top directory)
- Start BProlog:

    /pathtoBrolog/bp

- Compile and load `main.pl` (this will compile and load any other necessary files)

    ~~~
    | ?- cl('main.pl').
    ~~~

    You should see something like this:

    ~~~
    Compiling::main.pl
    compiled in 2 milliseconds
    loading::main.out
    Compiling::compl_struc.pl
    compiled in 5 milliseconds
    loading::compl_struc.out
    Compiling::rule_manip.pl
    compiled in 1 milliseconds
    loading::rule_manip.out
    Compiling::simple_colp.pl
    compiled in 1 milliseconds
    loading::simple_colp.out
    Compiling::expand.pl
    compiled in 7 milliseconds
    loading::expand.out
    Compiling::clash.pl
    compiled in 0 milliseconds
    loading::clash.out
    Compiling::show_result.pl
    compiled in 2 milliseconds
    loading::show_result.out
    ~~~

- Essentially we will be checking satisfiability of a predicate w.r.t. a logic
program under the open answer set semantics. You can get this `yes` or `no`
answer by trying for example:
    
    ~~~
    sat_raw(a,'./examples/input1.pl').
    ~~~

    where `a` is the predicate you are checking for satisfiability and the 2nd
    argument is the file containing the program (more about the syntax of programs
    below).

- Alternatively, you can obtain in addition to `yes/no` answer a graph that
indicates a tree model together with which nodes are blocking which other ones
(see the paper [RR 2009](http://stijnheymans.net/pubs/rr2009.pdf)). Try:

    ~~~
    sat_dot(a,'./examples/input1.pl').
    ~~~

    This will produce a file `TEMP.graph` in the current directory in
    [Graphviz](http://www.graphviz.org/) dot format.  

## Format of the Logic Programs

We follow a home-made syntax for expressing conceptual logic programs. Take for example the rule

    a(X) :- d(X), not e(X), f(X,Y), b(Y), g(X,Z), not b(Z).


This results in the following program:

    rule( r1, a, [ d, n(e)  ], [ [f, b ], [ g, n(b)] ]).
    
    unary( a ).
    unary( b ).
    unary( d ).
    unary( e ).
    binary( f ).
    binary( g ).
    
    free( f ).
    free( b ).
    free( g ).



Note `rule/4`. The first argument `r1` is the name of the rule.  The second argument `a` is the head predicate of the rule. The third argument is the predicates corresponding to the root `X` in the body. Thus `d(X)` becomes `d` and `not e(X)` becomes `n(e)`.  The final argument is a list of lists.  Each list in that list corresponds to the predicates for a successor in the body of the rule.  Thus, `f(X,Y), b(Y)` is represented by the list `[f,b]`, and `g(X,Z), not b(Z)` is represented by `[g,n(b)]`.

Finally, we indicate for each and every predicate whether it is `unary` or `binary` (note that because of how we encode the rule itself, this is necessary), and we indicate which of the predicates are `free`.




## License Information

All code published under the [WTFPL](http://sam.zoy.org/wtfpl/). Additionally,
no warranty whatsoever, as in

    This program is free software. It comes without any warranty, to the extent
    permitted by applicable law. You can redistribute it and/or modify it under the
    terms of the Do What The Fuck You Want To Public License, Version 2, as
    published by Sam Hocevar. See http://sam.zoy.org/wtfpl/COPYING for more
    details.

The above clause holds for each and every single file under this project (so
I am not repeating it in files specifically). Given that "no warranty" clause,
do whatever you want to with the code.

