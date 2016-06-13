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
#' @export
#' @examples
#' ws <- get_ws('C:/Documents/CH_exports/','Posts.*xls')


get_ws <- function(dir,patterns) {
  setwd(dir)
  fnames <- unlist(sapply(patterns, function(x) list.files(pattern=x)))
  print(fnames)
  ldply(fnames, function(x) XLConnect::readWorksheetFromFile(x,sheet=1))
}
