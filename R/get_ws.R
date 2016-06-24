#' Load an Excel worksheet
#'
#' This is just a wrapper function for XLConnect::readWorksheetFromFile()
#' for easier importing of Excel data.
#'
#' @param dir Directory in which files are stored
#' @param patterns One or more regex patterns to match files.
#'
#' Note that regex patterns are different from the glob (wildcard) patterns
#' that many users are accustomed to!
#' @examples
#' ws <- get_ws('data','GeoCenter 0524-1.xls')

#' @export
get_ws <- function(dir,patterns) {
  old_dir <- getwd()
  setwd(dir)
  fnames <- unlist(sapply(patterns, function(x) list.files(pattern=x)))
  print(fnames)
  res <- plyr::ldply(fnames, function(x) XLConnect::readWorksheetFromFile(x,sheet=1))
  setwd(old_dir)
  res
}
