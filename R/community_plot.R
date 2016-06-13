#' Plot network, highlighting communities
#'
#' In a social network, community-detection algorithms can be used to find
#' cliques that are more strongly connected to each other than to the rest
#' of the network. This generates a plot similar to sma_plot(), except that
#' the largest communities in the network are highlighted.
#'
#' @param g An igraph graph object.
#' @param c An igraph communities object.
#' @param n The number of mayors to find. By default, returns mayors for the
#' largest n communities.
#' @param layout Results of an igraph layout routine.
#' (layout.fruchterman.reingold is called if nothing is passed). Because these
#' calculations can be time consuming, it makes sense to obtain the layout in
#' advance if you'll be making several versions of the same plot.
#' @param showMayors If true, highlight the location of mayors for the
#' communities shown.
#' @param extra Additional communities that should be highlighted.
#'
#' @export
#' @examples
#' Add examples here.

community_plot <- function(g,c,n=8,layout=NULL,showMayors=TRUE,extra=NULL) {
  # plot a graph g using a community breakdown c
  # color in the first n communities
  if (is.null(layout)) {
    layout <- layout.fruchterman.reingold(g)
  }
  if (max(membership(c)) < n) {
    n <- max(membership(c))
  }
  t_all <- membership(c) %>% table() %>% as.data.frame()
  t_all <- t_all[order(t_all$Freq,decreasing=TRUE),]
  t <- head(t_all,n)
  for (i in 1:length(extra)) {
    t <- rbind(t,t_all[t_all$.==extra[i],])
  }
  col_n <- brewer.pal(nrow(t),'Dark2')
  V(g)$shape <- "none"
  V(g)$size <- 0
  V(g)$color <- "gray"
  source_nodes <- tail_of(g,E(g))
  target_nodes <- head_of(g,E(g))
  source_m <- membership(c)[source_nodes]
  target_m <- membership(c)[target_nodes]
  both_m <- rep(-1,length(E(g)))
  both_m[source_m==target_m] <- source_m[source_m==target_m]
  edge_colors <- rep("gray",length(E(g)))
  for (i in 1:nrow(t)) {
    edge_colors[both_m==t[i,1]] <- col_n[i]
  }
  E(g)$color <- edge_colors
  if (showMayors) {
    m <- mayors(g,c,n=n,extra=extra)
    for (i in 1:length(m)) {
      x <- m[i]
      V(g)[x]$shape <- "circle"
      V(g)[x]$size <- 4
      V(g)[x]$color <- col_n[i]
    }
  }
  plot(g,
       layout=layout,
       vertex.label=NA,
       vertex.shape="none",
       vertex.size=0,
       edge.arrow.mode=0,
       edge.width=1)
  # plot again, hiding the gray edges so that the colored ones are visible
  E(g)[E(g)$color=="gray"]$color  <- NA
  plot(g,
       layout=layout,
       add=TRUE,
       vertex.label=NA,
       edge.arrow.mode=0,
       edge.width=1)
}

# g_layout <- layout.fruchterman.reingold(g)
# community_plot(g,fg,layout=g_layout)
