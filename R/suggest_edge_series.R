#' Suggest an edge series
#'
#' The owner of a particular Twitter handle might want to increase his/her
#' standing in a particular conversation through targeted engagement with
#' specific handles. Given a graph metric to optimize, this function suggests
#' new interactions that will yield the greatest improvements.
#'
#' In general, this function is good for finding high-influence nodes that are
#' relatively distant in the network from a node of interest.
#'
#' TODO: I'm a little confused as to how I should export just one function from
#' here!
#'
#' @param g A graph object
#' @param n1 The name of the prospective retweeter
#' @param metric Graph metric to optimize
#' @param num_steps Number of iterations to perform
#' @param search_list List of possible targets to search over (defaults to V(g),
#' but choosing a shorter list of high-influence nodes will speed the search
#' considerably)
#' @param verbose Verbose output?
#' @param uniq_n2 Don't suggest the same partner more than once, even if
#' repeated interactions would continue to boost your metric of choice.
#'
#' @examples
#' library(dplyr)
#' library(igraph)
#' g <- get_ws('data','GeoCenter 0524-1.xls') %>%
#'   ws_to_graph() %>%
#'   graph_lcc()
#' slist <- page.rank(g)$vector %>%
#'   sort(decreasing=TRUE) %>%
#'   head(100) %>%
#'   names()
#' new_g <- suggest_edge_series(g,'@USAID',betweenness,5,search_list=slist)
#'
#' @import igraph
##############################################################################
# Suggest a series of edges that will maximize metric at each step. This
# should get us a more diverse set of suggested interaction partners than just
# a single step.
#
# If unique = TRUE, don't suggest the same node more than once.
###############################################################################
#' @export
suggest_edge_series <- function(g,n1,metric,num_steps,search_list=NULL,
                                verbose=TRUE,uniq_n2=TRUE) {
  if (is.null(search_list)) {
    search_list <- V(g)$name
  }
  new_g <- g
  if (verbose) {
    print(paste('initial',get_metric(g,metric,n1),sep=': '))
  }
  for (i in 1:num_steps) {
    n2_i <- suggest_edge(new_g,n1,metric,search_list,verbose=FALSE)
    new_g <- new_g + edge(n1,n2_i)
    if (uniq_n2) {
      search_list <- search_list[search_list != n2_i]
    }
    if (verbose) {
      print(paste(n2_i,get_metric(new_g,metric,n1),sep=': '))
    }
  }
  new_g
}

###############################################################################
# Wrapper function
###############################################################################
get_metric <- function(g,metric,n) {
  m <- metric(g)
  if ("vector" %in% names(m)) {
    return(m$vector[n])
  } else {
    return(m[n])
  }
}

###############################################################################
# Calculate metric for an alternate graph in which a new edge exists between
# n1 (retweeter) and n2 (source)
###############################################################################
try_edge <- function(g,n1,n2,metric) {
  new_g <- g + edge(n1,n2)
  get_metric(new_g,metric,n1)
}

###############################################################################
# Find the new edge originating at n1 (retweeter) that does the most to
# improve metric. If this search is prohibitively slow, offer a list of
# suggestions identified by some other means.
#
# I'm assuming here that we want to increase the value of the metric; might
# need to change that in the future.
###############################################################################
suggest_edge <- function(g,n1,metric,search_list=NULL,verbose=TRUE) {
  if (is.null(search_list)) {
    search_list <- V(g)$name
  }
  # set starting point
  m <- metric(g)
  best_val <- get_metric(g,metric,n1)
  best_n2 <- ''
  if (verbose==TRUE) {
    print(paste(best_n2,best_val))
  }
  for (n2 in search_list) {
    new_val <- try_edge(g,n1,n2,metric)
    if (new_val > best_val) {
      best_n2 <- n2
      best_val <- new_val
      if (verbose==TRUE) {
        print(paste(best_n2,best_val))
      }
    }
  }
  return(best_n2)
}


