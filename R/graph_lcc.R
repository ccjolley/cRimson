#' Largest connected component of a graph
#'
#' Many graphs, including social networks, include a "giant component",
#' meaning that most nodes in the network are connected to each other.
#' Many network metrics (e.g. betweenness centrality) are only meaningful
#' within a connected component, and connected graphs are easier to
#' visualize. This function returns the largest (weakly) connected
#' component of a graph.
#'
#' @param g igraph graph object.
#'
#' @examples
#' library(igraph)
#' g <- erdos.renyi.game(100,0.02)
#' lcc <- graph_lcc(g)
#' length(V(lcc))

#' @import igraph
#' @export
graph_lcc <- function(g) {
  connected <- clusters(g,mode='weak')
  lcc_v <- V(g)[connected$membership==which.max(connected$csize)]
  induced.subgraph(g,lcc_v)
}
