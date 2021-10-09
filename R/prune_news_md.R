#' Converts an NEWS.md file into a pkgdown formatted NEWS.md file
#'
#' @param pkg Package path.
#'
#' @param input The name of the input \file{NEWS.md} file.
#'
#' @param output The name of the output \file{NEWS.md} file.
#'
#' @param package The name of the package. If not specified, it is inferred
#' from the \file{DESCRIPTION} file.
#'
#' @return (invisible) The Markdown output.
#'
#' @importFrom utils file_test
#' @export
prune_news_md <- function(pkg = ".", input = "NEWS.md", output = NULL, package = NULL) {
  stopifnot(file_test("-f", input))

  stopifnot(file_test("-d", pkg))

  pathname <- file.path(pkg, input)
  stopifnot(file_test("-f", pathname))

  if (is.null(package)) {
    desc <- file.path(pkg, "DESCRIPTION")
    stopifnot(file_test("-f", desc))
    desc <- read.dcf(file = desc)
    package <- desc[, "Package"]
  }

  bfr <- readLines(input, warn = FALSE)
  
  pattern <- sprintf("^(([#]+)[[:space:]]+(%s|Version)[[:space:]]+(([[:digit:]]+[.-])+[[:digit:]]+)).*", package)
  idxs <- grep(pattern, bfr, ignore.case = TRUE)
  
  ## Nothing todo?
  prune <- (length(idxs) > 0)
  if (prune) {
    ## Drop part following version specification
    bfr[idxs] <- gsub(pattern, "\\1", bfr[idxs])
  
    ## Peel of one layer of headers?
    level <- gsub(pattern, "\\2", bfr[idxs])
    if (all(nchar(level) >= 2)) {
      bfr <- gsub("^#([#]+[[:space:]])", "\\1", bfr)
    }
  }

  if (!is.null(output)) writeLines(bfr, con = output)

  bfr <- paste(bfr, collapse = "\n")
  attr(bfr, "pruned") <- prune
  invisible(bfr)
}
