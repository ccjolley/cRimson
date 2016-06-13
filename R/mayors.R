#' Find "mayors" in a network
#'
#' In a social network, community-detection algorithms can be used to find
#' cliques that are more strongly connected to each other than to the rest
#' of the network. For each community, the "mayor" is the node with the highest
#' influence. Finding mayors can often be a useful way of figuring out
#' whether the community has a definable theme or group identity.
#'
#' @param g An igraph graph object.
#' @param comm An igraph communities object
#' @param n The number of mayors to find. By default, returns mayors for the
#' largest n communities.
#' @param metric Function to use in determining influence within a community
#' @param extra Additional communities whose mayors should be found.
#'
#' @export
#' @examples
#' Add examples here.

###############################################################################
mayors <- function(g,comm,n=8,metric=page.rank,extra=NULL) {
  # for a community breakdown c of a graph g, return the mayors of the top n
  # communities
  if (max(membership(comm)) < n) {
    n <- max(membership(comm))
  }
  t_all <- membership(comm) %>% table() %>% as.data.frame()
  t_all <- t_all[order(t_all$Freq,decreasing=TRUE),]
  t <- head(t_all,n)
  for (i in 1:length(extra)) {
    t <- rbind(t,t_all[t_all$.==extra[i],])
  }
  res <- sapply(1:nrow(t),function(i) {
    cnum <- t[i,1]
    vi <- V(g)[membership(comm)==cnum]
    gi <- induced.subgraph(g,vi)
    scores <- metric(gi)
    if ('vector' %in% names(scores)) { # account for different metrics
      scores <- scores$vector
    }
    names(scores)[which.max(scores)]
  })
  names(res) <- as.character(t[,1])
  res
}
