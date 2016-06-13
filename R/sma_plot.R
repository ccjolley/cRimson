#' Draw a network, highlighting key influencers
#'
#' This function draws a (potentially large) social network. A metric function
#' (by default igraph::page.rank) is used to identify up to eight top
#' influencers, and these are highlighted by edge coloring.
#'
#' @param g An igraph graph object.
#' @param n Number of nodes to highlight. The maximum value (8) is set by
#' the number of colors available from RColorBrewer.
#' @param highlight The results of a scoring function according to which
#' highlighted nodes should be chosen. Calls igraph::page.rank() if nothing
#' is passed. Calculating the scoring in advance lets you economize if you'll
#' be using the same scoring function for an sma_plot() call and an sma_bar()
#' call.
#' @param layout Results of an igraph layout routine
#' (layout.fruchterman.reingold is called if nothing is passed). Because these
#' calculations can be time consuming, it makes sense to obtain the layout in
#' advance if you'll be making several versions of the same plot.
#' @param extra A list of node names to be highlighted in addition to the
#' top-ranked ones.
#'
#' @export
#' @examples
#' Add examples here.


sma_plot <- function(g,n=8,highlight=NULL,layout=NULL,extra=NULL) {
  if (is.null(layout)) {
    layout <- layout.fruchterman.reingold(g)
  }
  if (is.null(highlight)) {  # use PageRank by default
    highlight <- page.rank(g)
  }
  if (length(extra) > n) {
    n <- length(extra)
  }
  col_n <- brewer.pal(n,'Dark2')
  if ("vector" %in% names(highlight)) {
    top <- highlight$vector %>% sort(.,decreasing=TRUE) %>% head(.,n-length(extra)) %>%
      c(.,highlight$vector[extra]) %>% names() %>% match(.,names(highlight$vector))
  } else {
    top <- highlight %>% sort(.,decreasing=TRUE) %>% head(.,n-length(extra)) %>%
      c(.,highlight[extra]) %>% names() %>% match(.,names(highlight))
  }
  E(g)$color <- "gray"
  V(g)$shape <- "none"
  V(g)$size <- 0
  V(g)$color <- "gray"
  for(i in 1:n) {
    x <- top[i]
    E(g)[incident(g,x,mode="all")]$color <- col_n[i]
    V(g)[x]$shape <- "circle"
    V(g)[x]$size <- 4
    V(g)[x]$color <- col_n[i]
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
