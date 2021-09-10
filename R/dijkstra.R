#' Dijkstra Algorithm
#' @param graph A graph in which to search for the shortest paths
#' @param init_node The starting node from which the shortest paths are calculated
#' @description The algorithm finds the distance  of the shortest path between a given node in a graph and every other node in the graph
#' @return Returns a vector containing the distance of the shortest path between the given starting node to each other node.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @export

dijkstra <-
function(graph, init_node){
  if(!(names(graph)[1] == "v1"&& names(graph)[2] == "v2" && names(graph)[3] =="w")){
    stop("column names of the input graph must be v1, v2 and w")
  }
  else if (!(init_node %in% unlist(graph[1], graph[2]))){
    stop("init_node must be in the input graph")
  }
  else{
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
    return(as.numeric(dist)) 
  }
}
