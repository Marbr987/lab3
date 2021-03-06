% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dijkstra.R
\name{dijkstra}
\alias{dijkstra}
\title{Dijkstra Algorithm}
\usage{
dijkstra(graph, init_node)
}
\arguments{
\item{graph}{A graph in which to search for the shortest paths}

\item{init_node}{The starting node from which the shortest paths are calculated}
}
\value{
Returns a vector containing the distance of the shortest path between the given starting node to each other node.
}
\description{
The algorithm finds the distance  of the shortest path between a given node in a graph and every other node in the graph
}
\references{
https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
}
