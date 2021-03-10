#' Converts a traditional NEWS files into a Markdown NEWS.md File
#'
#' @param pkg Package path.
#'
#' @param input The name of the input \file{NEWS} file.
#'
#' @param output The name of the output \file{NEWS.md} file.
#'
#' @param overwrite If TRUE, an existing output file is overwritten.
#'
#' @param package The name of the package. If not specified, it is inferred
#' from the \file{DESCRIPTION} file.
#'
#' @param style The style of the \file{NEWS.md} file.
#'
#' @param category_case The style of the category headers.
#'
#' @param escape If TRUE, symbols that have special meaning in Markdown are
#' escaped.
#'
#' @return (invisible) The Markdown output.
#'
#' @importFrom utils file_test
#' @importFrom tools toTitleCase
#' @export
news_to_md <- function(pkg = ".", input = "NEWS", output = "NEWS.md", overwrite = FALSE, package = NULL, style = c("NEWS", "pkgdown"), category_case = c("TitleCase", "as-is"), escape = TRUE) {
  toTitleCase <- tools::toTitleCase
  news_reader_default <- import_from("tools", ".news_reader_default")
  
  style <- match.arg(style)
  category_case <- match.arg(category_case)
  
  stopifnot(file_test("-d", pkg))

  pathname <- file.path(pkg, input)
  stopifnot(file_test("-f", pathname))

  if (is.null(package)) {
    desc <- file.path(pkg, "DESCRIPTION")
    stopifnot(file_test("-f", desc))
    desc <- read.dcf(file = desc)
    package <- desc[, "Package"]
  }

  news <- news_reader_default(pathname)

  if (is.character(output)) {
    stopifnot(overwrite || !file_test("-f", output))
  } else {
    stopifnot(inherits(output, "connection"))
  }

  ## Sanity check
  bad <- which(attr(news, "bad"))
  if (length(bad) > 0) {
    news_bad <- news[bad, ]
    msg <- sprintf("Detected %d malformed entries in %s: %s",
                   nrow(news_bad), sQuote(pathname),
                   paste(news_bad$Version, collapse = ", "))
    stop(msg)
  }

  ## Split up in releases
  releases <- split(news, news$Version)

  ## Preserve order according to NEWS
  if (length(releases) > 1) {
    releases <- releases[unique(news$Version)]
  }

  mds <- lapply(releases, FUN = function(release) {
    version <- unique(release$Version)
    stopifnot(length(version) == 1L)
    
    date <- unique(release$Date)
    if (length(date) > 1L) {
      stop(sprintf("Syntax error in NEWS: Release version %s has more than one date: %s", sQuote(version), commaq(date)))
    }

    if (style == "NEWS") {
      header <- sprintf("## Version %s", version)
    } else if (style == "pkgdown") {
      header <- sprintf("# %s %s", package, version)
    }
    
    if (nzchar(date) && style != "pkgdown") {
      header <- sprintf("%s [%s]", header, date)
    }

    ## Split up in categories
    categories <- split(release, release$Category)
    
    ## Preserve order according to NEWS
    if (length(categories) > 1) {
      categories <- categories[unique(release$Category)]
    }

    mds <- lapply(categories, FUN = function(category) {
      title <- unique(category$Category)
      stopifnot(length(title) == 1L)

      if (category_case == "TitleCase") {
        title <- tolower(title)
        title <- toTitleCase(title)
      }
      
      header <- sprintf("### %s", title)
      texts <- category$Text

      ## Drop newlines?
      if (FALSE) {
        texts <- lapply(texts, FUN = function(text) {
          text <- strsplit(text, split = "[\n\r]", fixed = FALSE)
          text <- unlist(text, use.names = FALSE)
          text <- paste(text, collapse = " ")
          text
        })
        texts <- unlist(texts, use.names = FALSE)
      }

      ## Escape Markdown?
      if (escape) {
        texts <- gsub("_", "\\_", texts, fixed = TRUE)
        texts <- gsub("*", "\\*", texts, fixed = TRUE)
        texts <- gsub("`", "\\`", texts, fixed = TRUE)
      }

      items <- sprintf("* %s", texts)
      mds <- c(header, items)
      mds <- unlist(rbind(mds, ""), use.names = FALSE)
      mds
    })

    mds <- c(header, mds)
    mds <- unlist(rbind(mds, ""), use.names = FALSE)
  })

  if (style == "NEWS") {
    header <- sprintf("# Package %s", sQuote(package))
  } else if (style == "pkgdown") {
    header <- NULL
  }

  mds <- c(header, mds)
  
  mds <- unlist(mds, use.names = FALSE)
  writeLines(mds, con = output)
  
  mds <- paste(mds, collapse = "\n")
  invisible(mds)
}
