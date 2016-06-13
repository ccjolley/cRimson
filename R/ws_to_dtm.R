###############################################################################
ws_to_dtm <- function(ws) {
  # Beginning with an Excel worksheet from Crimson Hexagon, generate a
  # document-term matrix in which urls, excess whitespace, most punctuation, and
  # English stopwords have been removed.
  # Keeping '@' and '#' characters because of their special Twitter meaning.
  # Also convert to lowercase and keep only terms appearing in 0.01% or more
  # of documents.
  corpus <- VCorpus(VectorSource(ws$Contents))
  no_url <- content_transformer(function(x) gsub('https?:\\/\\/[a-zA-Z0-9.\\/]*','',x))
  corpus <- tm_map(corpus,no_url)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,content_transformer(tolower))
  # This doesn't work quite right yet
  mostPunct <- content_transformer(function(x)
    gsub('[!"$%&\'()*+\\,\\.\\/:;<=>?\\^_`\\{\\|\\}~]',' ',x))
  corpus <- tm_map(corpus,mostPunct)
  #corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  dtm <- DocumentTermMatrix(corpus)
  removeSparseTerms(dtm,0.999)
}

# TODO: make the text processing steps options, rather than hard-coded in
