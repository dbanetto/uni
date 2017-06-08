% Assignment 2 - Small World Graph - SWEN421 
% David Barnett (300313764)

# Documentation

## Overview

The main package is `Graphs`, which is generic.

`Graphs` has only one generic parameter, which is the capacity of nodes
in the graph via `Node_Capcity`.
The graph is directed with uniformly weighted edges and does
not allow for user data to be held in the graph via weights or node.
This package focuses on properties of the graph
via the structure instead of handling the contents of it.

An example of initialising a graph:

```ada
pacakge My_Graphs is new Graphs(Node_Capcity => 10);
my_graph : My_Graphs.Graph := My_Graphs.Empty_Graph;
```

This does not follow the general convention of the Formal Containers in Ada where
the capacity is set by a discriminate when sub-typing the container.
However, I think this looks cleaner and allows for use of dot notation on a `Graph` instance.

## Structure

The internal structure of the graph is a set of edges in the graph and a count of nodes
created. 
The nodes are completely controlled by the graph package ; the user cannot create
their own nodes, achieved via making `Node_Label` a private type, and must ask the graph for
a new node (ref: `New_Node`).
Deletion of a node is not allowed.
Internally `Node_Label` is an integer which each label being an increment, this allows
for all nodes be contained in the range `1..Node_Count`.

Edges are stored in a set and are directed with uniformly weighted (all one).
An implication of using a set to hold them is that there are no duplicate edges, 
which also saves space since in this graph package it is redundant as they cannot differ with
weights.
The capacity of edges is derived from the maximum edges allowed in a graph with `Node_Capcity`
number of nodes.

```ada
my_graph.New_Node(node_1);
my_graph.New_Node(node_2);

my_graph.Add_Edge(node_1,  node_2);
```

The structure of the proofs are to build up from basic facts such as "node is in the graph" and "has
an edge between" to paths. Some of the proofs are inductive (such as `Has_Path`) which have been
fine with Spark compiling but have not been able to get the prover to work enough to check if they
are valid for it.

There is no use of flow contracts as there is no global state used.
The only global variables (`Edge_Capacity` and `Node_Capcity`\*) are also constants.
All state are held in the `Graph` records.

> \* when the package is changed into non-generic.

## Sub-Programs

### General Usage

To add nodes to the graph you must first ask for a node from the graph with:

`New_Node( self : in out Graph ; node : out Node_Label )`

This is the only way to get a valid node for a graph.

To add an edge you pass a pair of nodes:

`Add_Edge( self : in out Graph ; from, to : Node_Label )`

### Distance

Finds the shortest path between the two nodes in the direction of `from` to `to`.
The distance function is how many edges were traversed to get the final `to` node.

This has the precondition that there exists a path between `from` and `to`.

`Distance_Between (self : Graph ; from, to : Node_Label ) return Distance`

Internally this is using a modified Dijkstra's algorithm to find the shortest path without
retaining the path used.

### Diameter

Finds the maximum distance between any two nodes.

There is no preconditions for `diameter`.

`Diameter (self : Graph ) return Distance`

Internally this uses full Dijkstra's algorithm to find the shortest path from
one source to all reachable nodes and find the maximum of those distances from each
node.

### Small

Determines if the graph is a "small work graph" by comparing the diameter against 
the number of nodes in the graph times a given coefficient.

There is no preconditions for `small`.

`Small (self : Graph ; coefficient Float) return Boolean`

# Justification of quality of Code

No provable justification can be given since GNATProve consistently has internal errors when
processing this package.

The only proof that it is correct is running the test code that builds a graph and 
checks all the properties (edge relations, paths, distances, diameter, and small(x) ).

## Bugs Reports

These are bugs that have stopped me from being able to run GNATProve on the
whole project, in each instance they can build and run correctly but just not prove
at any level.
If the multiprocessing flag is set some proofs are calculated for `test.adb` but not `graphs.ads`
or `graphs.adb`

When the package is setup to be generic and proven the following error messages appears: 

```log
Phase 1 of 2: generation of Global contracts ...
+===========================GNAT BUG DETECTED==============================+
| GPL 2016 (20160515) (spark) Program_Error spark_definition.adb:1929 explicit raise|
| Error detected at graphs.ads:16:34 [test.ads:5:4]                        |
| Please submit a bug report by email to report@adacore.com.               |
| GAP members can alternatively use GNAT Tracker:                          |
| http://www.adacore.com/ section 'send a report'.                         |
| See gnatinfo.txt for full info on procedure for submitting bugs.         |
| Use a subject line meaningful to you and us to track the bug.            |
| Include the entire contents of this bug box in the report.               |
| Include the exact command that you entered.                              |
| Also include sources listed below.                                       |
| Use plain ASCII or MIME attachment(s).                                   |
+==========================================================================+

Please include these source files with error report
Note that list may not be accurate in some cases,
so please double check that the problem can still
be reproduced with the set of files listed.
Consider also -gnatd.n switch (see debug.adb).

src/play.adb
src/test.ads
src/graphs.ads
src/graphs.adb
src/logger.ads

compilation abandoned
gnatprove: error during generation of Global contracts
[2017-06-07 11:12:18] process exited with status 1, elapsed time: 00.44s
```

When converted into a non-generic package (the `Node_Capcity` is replaced with a constant value)
a different error occurs but still stops all proving.
I have found the error reporting for the generic version is far worse than non-generic packages.

```log
Phase 1 of 2: generation of Global contracts ...
Phase 2 of 2: flow analysis and proof ...
Internal error
File "build/gnatprove/graphs/../graphs.mlw", line 17682, characters 72-122:
unbound symbol 'Graphs__edgeset__element_type.element_type__content'.
gnatprove: error during flow analysis and proof
[2017-06-07 11:13:18] process exited with status 1, 25% (1/4), elapsed time: 05.79s
```


During the development process I also came across a strange error
when the private type `Nodel_Label` with generic containers after the
concrete definition is given.
It is solved by making a sub-type of `Nodel_Label` and using that instead.
The stranger part was it was fine for `Egde` 
to be used directly but not `Node_Label`, below is the code and the error.

```Ada
   type Node_Label is new Count_Type;
   subtype Node_T is Node_Label;

    -- ommited

    -- this will fail with the error below
   package NodeDistance is new Formal_Ordered_Maps(Key_Type => Node_Label,
                                                   Element_Type => Distance);
    -- this will be fine
   package NodeDistance is new Formal_Ordered_Maps(Key_Type => Node_T,
                                                   Element_Type => Distance);
```

```log
Phase 1 of 2: generation of Global contracts ...
Phase 2 of 2: flow analysis and proof ...
Internal error
File "build/gnatprove/graphs/../graphs.mlw", line 6959, characters 82-84:
This term has type node_label, but is expected to have type int.
gnatprove: error during flow analysis and proof
[2017-06-07 11:14:32] process exited with status 1, 25% (1/4), elapsed time: 05.54s
```

## Code coverage

Below is a coverage report of the `play` binary.

Some of the lines not used are type definitions which has been confusing.

File       | Coverage
-----------|----------:
logger.adb | 100.00% of 18
play.adb   | 100.00% of 3
test.ads   | 100.00% of 2
graphs.ads | 75.00% of 12
test.adb   | 97.50% of 40
graphs.adb | 84.68% of 111
**Total**  | **89% of 185**

Coverage report of the usage of library code used:

Library      | Coverage
-------------|--------------
a-cforse.adb | 30.23% of 344
a-rbtgbk.adb | 44.05% of 168
a-rbtgbo.adb | 32.44% of 373
a-btgbso.adb | 12.41% of 266
a-cforma.adb | 24.65% of 288
a-conhel.adb | 36.96% of 46
s-atocou.adb | 100.00% of 3
a-conhel.ads | 66.67% of 6
a-crbltr.ads | 60.00% of 5
a-cforse.ads | 66.67% of 12
a-cforma.ads | 85.71% of 7
s-stoele.adb | 0.00% of 3
**Total**    | **35.68% of 1707**

# Known Weakness of the Code

It is slow.
Every path is calculated by using full Dijkstra's Algorithm to every node.
The destination is just plucked out of the distance map
(which maps node -> shortest distance from source) at the end.

It has not been fully proven by GNATProve for the bugs or error encountered above.
All contracts have been made to the best guess I can make without being able to run them.

The proofs given are probably not complete and not run, I have done some whiteboard 
walk through of the key proofs, such as `Has_Path` and `Distance_Between`

For the sub-programs that do require pre-conditions they are also programmed to
be robust when they are not met.
