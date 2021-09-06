#' Dijkstra Algorithm
#' @param graph A graph in which to search for the shortest paths
#' @param init_node The starting node from which the shortest paths are calculated
#' @description The algorithm finds the distance  of the shortest path between a given node in a graph and every other node in the graph
#' @return Returns a named vector containing the distance of the shortest path between the given starting node to each other node. The distance to the specific nodes is indicated by the name in the returned vector.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm



dijkstra <- function(graph, init_node){
  vertexSet_Q <- c()
  dist <- c()
  prev <- c()
  for (vertex in unique(unlist(list(graph$v1, graph$v2)))) {
    dist[vertex] <- Inf
    prev[vertex] <- 'NULL'
    vertexSet_Q <- append(vertexSet_Q, vertex)
  }
  names(prev) <- vertexSet_Q
  names(dist) <- vertexSet_Q
  dist[names(dist) == init_node] <- 0
  while(length(vertexSet_Q) != 0){
    vertex_u <- as.numeric(names(dist)[dist == min(unlist(dist[names(dist) %in% vertexSet_Q]))])
    vertexSet_Q <- vertexSet_Q[!(vertexSet_Q %in% vertex_u)]
    neighbors_of_u <- unique(unlist(list(graph$v2[graph$v1 %in% vertex_u], graph$v1[graph$v2 %in% vertex_u])))
    neighbors_of_u_in_Q <- neighbors_of_u[neighbors_of_u %in% vertexSet_Q]
    for (neighbor_v in neighbors_of_u_in_Q) {
      alt <- dist[names(dist) == vertex_u] + graph$w[(graph$v1 == vertex_u & graph$v2 == neighbor_v)]
      if(alt < dist[names(dist) == neighbor_v]){
        dist[names(dist) == neighbor_v] <- alt
        prev[names(dist) == neighbor_v] <- vertex_u
      }
    }
  }
  return(dist)
}
