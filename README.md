##Dijsktra's SSSP Algorithm

Graphs are being read from an adjacency list into a cyclic linked list of nodes.
The `dijkstra` function serves as a wrapper around linking the data structure and the actual graph traversal.
The `traverse` function poses the only significant change between the different versions:

- **fold** folds the list of nodes with the candidate and visited set as the aggegator.

- **ioref** uses mutable IORefs for the candidate and visited set.

- **rec** uses an explicitly recursive function to traverse the graph.

- **state** traverses the graph recursively using the State Monad.

##Todo

- Visualize internal graph represantation using [ghc-vis](https://github.com/def-/ghc-vis)
- Profiling/Optimization of the `dijkstra` & `traverse` functions
- Benchmarking using [criterion](https://github.com/bos/criterion)
