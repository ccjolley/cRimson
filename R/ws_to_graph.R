#' Convert a worksheet from Crimson Hexagon to an igraph object
#'
#' This function converts Excel worksheet containing a Crimson Hexagon data
#' export to a directed graph. Nodes are named after Twitter handles (including
#' the '@' prefix) and edges point from a retweeter to a retweetee.
#'
#' @details TODO: it might be handy to include the ability to build a graph from
#' mentions as well, rather than just retweets.
#'
#' @param ws Data frame returned, for example, by get_ws()
#'
#' @examples
#' library(dplyr)
#' g <- sample_tweets %>%
#'     ws_to_graph()
#'
#' @author Craig Jolley, cjolley.usaid@gmail.com

#' @export
ws_to_graph <- function(ws){
  wsRT <- ws[grep("^RT ",ws$Contents),] # keep only retweets
  wsRT <- unique(wsRT)
  edges <- data.frame(rt=wsRT$Author,
                      orig=sapply(strsplit(wsRT$Contents,' '),
                                  function(x) gsub(':$','',x[2],perl=TRUE)))
  igraph::graph.data.frame(edges,directed=TRUE)
}


