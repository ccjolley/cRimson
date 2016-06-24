#' Bar chart showing network influencers
#'
#' This function is used together with sma_plot(), and gives a color-coded
#' guide to the top influencers and their relative influence scores.
#'
#' @param g An igraph graph object.
#' @param n Number of influencers to display. The maximum value (8) is set by
#' the number of colors available from RColorBrewer.
#' @param highlight The results of a scoring function according to which
#' highlighted nodes should be chosen. Calls igraph::page.rank() if nothing
#' is passed.  Calculating the scoring in advance lets you economize if you'll
#' be using the same scoring function for an sma_plot() call and an sma_bar()
#' call.
#' @param extra A list of node names to be highlighted in addition to the
#' top-ranked ones.
#'
#' @examples
#' library(dplyr)
#' get_ws('data','GeoCenter 0524-1.xls') %>%
#'     ws_to_graph() %>%
#'     sma_bar()

#' @import igraph
#' @export
sma_bar <- function(g,n=8,highlight=NULL,extra=NULL) {
  if (is.null(highlight)) {  # use PageRank by default
    highlight <- page.rank(g)
  }
  if (length(extra) > n) {
    n <- length(extra)
  }
  if ("vector" %in% names(highlight)) {
    prtop <- highlight$vector %>% sort(.,decreasing=TRUE) %>%
      head(.,n-length(extra)) %>% c(.,highlight$vector[extra])
  } else {
    prtop <- highlight %>% sort(.,decreasing=TRUE) %>%
      head(.,n-length(extra)) %>% c(.,highlight[extra])
  }
  prdf <- data.frame(name=names(prtop),pr=prtop)
  # re-order
  prdf$name <- factor(prdf$name,levels=rev(as.character(prdf$name)))
  col_n <- brewer.pal(n,'Dark2')
  prdf$color <- col_n
  ggplot(prdf,aes(x=name,y=pr)) +
    geom_bar(stat='identity',fill=rev(col_n),alpha=0.4) +
    coord_flip() +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.x=element_blank(),
          axis.line.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank())
}
