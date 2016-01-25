# 2D-Bin-Packing-Solver

This project implements several algorithms and strategies to solve bin packing problems.

## Setup 

To use this you just need to load the basic searches file, compile the bin-packing file and load it. Example:

    > (load "basic-searches")
    > (compile-file "bin-packing.lisp")
    > (load "bin-packing")
    > (place-pieces ....)
  
## Execution

There are a couple of pre-defined problems with different degrees of difficulty (see the last section). In a bin-packing problem, a hard problem is one in which the solution is very tight and, therefore, harder to achieve. A problem is essentialy 
a list of pieces (that only have a width and a height to begin with) and the dimensions of the "board". To run the program invoke the fuction "place-pieces" with a problem and a search strategy. Like this:

    > (place-pieces p1c "best.approach.satisfaction")

This will return a list of pieces. At this point, in addition to its dimensions, each piece will have an orientation (horizontal or vertical) 
and a position. You can use the "draw-board" function to validate the solution. For instance:
    
<pre>>(draw-board (place-pieces p1c "best.approach.satisfaction"))

13 13 13 13  3  3 14 14  2  2 15 15  1  1  1  1  1
13 13 13 13  3  3 14 14  2  2 15 15  1  1  1  1  1
13 13 13 13  3  3 14 14  2  2 15 15 12 12 12  4  4
 6  6 6  6  11 11 11 11  5  5  5  5 12 12 12  4  4
 6  6  6  6 11 11 11 11  5  5  5  5 12 12 12 10 10
 6  6  6  6 11 11 11 11  5  5  5  5 12 12 12 10 10
 8  8  8  8  8  9  9  9  9  9  7  7  7  7  7 10 10
 8  8  8  8  8  9  9  9  9  9  7  7  7  7  7 10 10
 8  8  8  8  8  9  9  9  9  9  7  7  7  7  7 10 10
 NIL</pre>

As you can see, this solution is absolutely compact and there is no unused space. This isn't the case in an easier, less compact, problem:
<pre>> (draw-board (place-pieces p1c "best.approach.satisfaction"))

[] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []
[] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []
[] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []
[] [] [] []  1  1 [] [] [] [] [] [] [] [] []  4  4
13 13 13 13  1  1 [] [] [] []  2  2 15 15 15  4  4
13 13 13 13  3  3  3 14 14 14  2  2 15 15 15  4  4
13 13 13 13  3  3  3 14 14 14  2  2 12 12 12  4  4
 6  6  6  6 11 11 11 11  5  5  5  5 12 12 12  4  4
 6  6  6  6 11 11 11 11  5  5  5  5 12 12 12 10 10
 6  6  6  6 11 11 11 11  5  5  5  5 12 12 12 10 10
 8  8  8  8  8  9  9  9  9  9  7  7  7  7  7 10 10
 8  8  8  8  8  9  9  9  9  9  7  7  7  7  7 10 10
 8  8  8  8  8  9  9  9  9  9  7  7  7  7  7 10 10
NIL</pre>

### Algorithms

The implemented strategies are divided in optimization strategies and satisfaction strategies. Satisfaction strategies return any valid solution 
(a list of non-overlapping pieces that fit in a pre-defined board). Optimization strategies ignore the provided height and return the
most compact solution possible (many algorithms can't actually do this, most just return a good solution - not the best).

The list of implemented satisfaction strategies are the following: 
* **a\*.best.heuristic** -  an A* search with the best heuristic developed
* **best.approach.satisfaction** - the same as the previous algorithm
* **a\*.best.alternative.heuristic** - an A* search with the second best heuristic
* **iterative.sampling.satisfaction** - an iterative sampling search (only useful when there are many solutions; doesn't do well in tight boards)
* **ILDS** - an [Improved Limited Discrepancy Search](http://aaaipress.org/Papers/AAAI/1996/AAAI96-043.pdf) (interesting and pretty good)

The list of implemented optimization strategies are the following: 

* **best.approach.optimization** - an A* search with an optimization goal
* **iterative.sampling.optimization** - an iterative sampling search (this search quickly finds good solutions but not the best)

It's important to note that most of the code complexity is in the operator function ("operador"). It's in this function that an efficient representation
is used to make this problem solvable. This function also reduces the combinatorial explosion of states by making certain simplifications and 
optimizations. This is particularly important when the A* algorithm is used.

## Pre-defined problems

There are three types of pre-defined problems (and 4 of each):
* Easy problems - p[1-4]c
* Hard problems - p[1-4]b
* Impossible problems - p[1-4]a
