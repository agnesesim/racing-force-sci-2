library(tidyverse)
library(igraph)
library(fs)

# Nodes
actors <- data.frame(
  name = c("Alice", "Bob", "Cecil", "David", "Esmeralda"),
  age = c(48, 33, 45, 34, 21),
  gender = c("F", "M", "F", "M", "F")
)
# Edges
relations <- data.frame(
  from = c("Bob", "Cecil", "Cecil", "David", "David", "Esmeralda"),
  to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
  sameDept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
  friendship = c(4, 5, 5, 2, 1, 1),
  advice = c(4, 5, 5, 4, 2, 3)
)

g = graph_from_data_frame(relations, directed = TRUE, vertices = actors)

# Nodes and node count
V(g)
vcount(g)

# Edges and edge count
E(g)
ecount(g)

# From igraph to datafra,e
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")
as_data_frame(g, what="both")

# sparse matrix
as_adjacency_matrix(g)

# non-sparse matrix
as_adjacency_matrix(g, sparse = FALSE)

# weighted matrix
as_adjacency_matrix(g, attr = "friendship")

# From adjacency matrix to igraph
A = as_adjacency_matrix(g)
graph_from_adjacency_matrix(A)


# Read and write graph on disk
# write to graphml format
write_graph(g, file="agnese/Alice.xml", format="graphml")
# read from graphml format (notice the new id vertex attribute)
read_graph(file="agnese/Alice.xml", format="graphml")

# plot graph defining visualization parameters for graph, nodes and edges
p <- plot(g,
          vertex.size = 20,
          vertex.color = "white",
          vertex.shape = "square",
          vertex.label = letters[1:vcount(g)],
          edge.width = 2,
          edge.color = "black",
          edge.lty = 3,
          edge.label = LETTERS[1:ecount(g)],
          edge.curved = TRUE)


# undirect graph
edges = c(1,2, 1,5, 2,3, 2,4, 3,4, 3,5, 3,6)
ug = graph(edges, directed=FALSE)
coords = layout_with_fr(ug)

# direct graph
edges = c(1,3, 1,4, 2,5, 3,2, 4,4, 4,5, 5,3, 5,6, 5,4)
dg = graph(edges, directed=TRUE)
coords = layout_with_fr(dg)

# weighted graph
edges = c(1,2, 1,5, 2,3, 2,4, 3,4, 3,5, 3,6)
wg = graph(edges, directed=FALSE)
E(wg)$weight = c(1, 5, 2, 8, 4, 5, 6)
coords = layout_with_fr(wg)

# bipartite graph
parts = c(rep(TRUE,7), rep(FALSE,4))
edges = c(8,1, 8,2, 8,3, 9,2, 9,3, 9,4, 9,5, 10,4, 10,6, 11,5, 11,6, 11,7)
bg = make_bipartite_graph(parts, edges, directed=FALSE)
lay = layout.bipartite(bg)

### Centrality measure

## degree
# function degree of package igraph computes degree centrality
edges = c(1,3, 1,4, 2,5, 3,2, 4,4, 4,5, 5,3, 5,6, 5,4)
dg = graph(edges, directed=TRUE)
coords = layout_with_fr(dg)

table_degree <- data.frame(
  Node = c(1,2,3,4,5,6),
  Degree = degree(dg, mode = "total"),
  Indegree = degree(dg, mode = "in"),
  Outdegree = degree(dg, mode = "out")
)

# function strength of package igraph computes degree centrality.
edges = c(1,2, 1,5, 2,3, 2,4, 3,4, 3,5, 3,6)
wg = graph(edges, directed=TRUE)
E(wg)$weight = c(1, 5, 2, 8, 4, 5, 6)
coords = layout_with_fr(wg)

table_strength <- data.frame(
  Node = c(1,2,3,4,5,6),
  Degree = strength(wg, mode = "total"),
  Indegree = strength(wg, mode = "in"),
  Outdegree = strength(wg, mode = "out")
)


## closeness : A node is important based on how it is close to all the other nodes in network.


## betweeness : A node is important based on the amount of influence a node has over the flow in a graph.


# eigenvector : A node is important if it is linked to by other important nodes
