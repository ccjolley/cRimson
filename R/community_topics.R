###############################################################################
community_topics <- function(c,ws,dtm=NULL,n=8,num_keywords=10,extra=NULL,
                             cdtm=NULL) {
  # given a document-term matrix and a community breakdown, aggregate documents
  # to return a new dtm with only one document per community.
  # For each of the top n communities in a clustered dtm, output the terms
  # with the highest TF-IDF weight
  if (is.null(dtm) & is.null(cdtm)) {
    print('ERROR in community_topics(): Must specify either dtm or cdtm!')
    return(NULL)
  }
  if (is.null(cdtm)) {
    cdtm <- cluster_dtm(dtm,c,ws)
  }
  t_all <- membership(c) %>% table() %>% as.data.frame()
  t_all <- t_all[order(t_all$Freq,decreasing=TRUE),]
  t <- head(t_all,n)
  for (i in 1:length(extra)) {
    t <- rbind(t,t_all[t_all$.==extra[i],])
  }
  dtm_w <- as.DocumentTermMatrix(cdtm,weighting=weightTf) %>% weightTfIdf()
  res <- sapply(t$.,function(x) {
    row <- data.frame(name=colnames(dtm_w),
                      val=t(as.matrix(dtm_w[as.character(x),])))
    row$name <- as.character(row$name)
    row <- row[order(row[,2],decreasing=TRUE),]
    head(row$name,10)
  })
  colnames(res) <- as.character(t$.)
  res
}

# TODO: if neither a dtm nor a cdtm gets passed in, just start from the worksheet
# and generate everything.
