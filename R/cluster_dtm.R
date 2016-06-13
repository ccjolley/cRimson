###############################################################################
cluster_dtm <- function(dtm,c,ws) {
  # use this to save time if you'll be running community_topics() repeatedly
  m <- as.matrix(dtm)
  clust_assign <- membership(c)[ws$Author]
  clust_assign[is.na(clust_assign)] <- -1
  cluster.list <- by(m, clust_assign, colSums)
  cluster.dtm <- matrix(unlist(cluster.list), nrow = length(cluster.list), byrow = T)
  colnames(cluster.dtm) <- names(cluster.list[[1]])
  rownames(cluster.dtm) <- names(cluster.list)
  cluster.dtm
}

# TODO: make passing a dtm optional; since you're passing ws anyway you
# may as well do both steps at once.
