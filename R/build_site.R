#' Build a complete 'pkgdown' website
#'
#' @details
#' This function is an enhancement to [pkgdown::build_site()] with the
#' same name.
#'
#' @param pkg Path to package.
#'
#' @param \ldots ... Additional arguments passed to [pkgdown::build_site()].
#'
#' @param github If TRUE, GitHub specific files are created, e.g. CNAME.
#'
#' @param preview Preview site in browser?
#'
#' @returns Nothing.
#'
#' @importFrom desc desc_get_field
#' @importFrom pkgbuild build
#' @importFrom utils file_test untar packageVersion
#' @importFrom tools pkgVignettes vignetteEngine file_path_sans_ext file_ext
#' @importFrom yaml read_yaml write_yaml
#' @export
build_site <- function(pkg = ".", ..., github = TRUE, preview = NA) {
  rule <- import_from("pkgdown", "rule")
  cat_line <- import_from("pkgdown", "cat_line")
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  build_github_pages <- import_from("pkgdown", "build_github_pages")
  preview_site <- import_from("pkgdown", "preview_site")
  oopts <- options(width = 80L)
  on.exit(options(oopts))
  
  stopifnot(file_test("-d", pkg))

  pkgname <- desc_get_field("Package")

  rule("Preprocessing package for pkgdown", line = "=")

  ## Compile _pkgdown.yml.rsp, if it exists
  pathname_yml <- build_pkgdown_yml()
  config <- if (is.null(pathname_yml)) NULL else read_yaml(pathname_yml)

  build_root <- tempdir()
  build_path <- file.path(build_root, pkgname)
  
  cat_line("Create shim package folder ", dst_path(build_path))
  tarball <- pkgbuild::build(
    path = pkg,
    dest_path = tempdir(),
    vignettes = FALSE,
    args = c("--no-resave-data", "--no-manual"),
    quiet = TRUE
  )

  ## Create intermediate package folder
  if (file_test("-d", build_path)) unlink(build_path, recursive = TRUE)
  stopifnot(!file_test("-d", build_path))
  res <- untar(tarball, exdir = tempdir())
  stopifnot(res == 0)
  stopifnot(file_test("-d", build_path))

  vignettes_path <- file.path(pkg, "vignettes")
  if (file_test("-d", vignettes_path)) {
    files <- dir(vignettes_path, pattern = "[.]pdf$", ignore.case = TRUE, full.names = TRUE)
    for (file in files) {
      cat_line("Copying vignette file ", src_path(file))
      file.copy(file, file.path(build_path, file))
      stopifnot(file_test("-f", file.path(build_path, file)))
    }
  }

  ## Copy man/
  pkgdown_path <- file.path(pkg, "man")
  if (file_test("-d", pkgdown_path)) {
    cat_line("Copying pkgdown folder ", src_path("man"))
    file.copy(pkgdown_path, build_path, recursive = TRUE)
    target_path <- file.path(build_path, "man")
    stopifnot(file_test("-d", target_path))
  }

  ## Copy all *.md files
  for (file in dir(path = pkg, pattern = "[.]md$")) {
    pkgdown_path <- file.path(pkg, file)
    target_path <- file.path(build_path, file)
    if (file_test("-f", pkgdown_path) && !file_test("-f", target_path)) {
      cat_line("Copying file ", src_path(file))
      file.copy(pkgdown_path, build_path)
      stopifnot(file_test("-f", target_path))
    }
  }

  ## Prune README.md
  file <- file.path(build_path, "README.md")
  if (file_test("-f", file)) {
    cat_line("Pruning file ", dst_path(basename(file)))
    bfr <- readLines(file, warn = FALSE)
    pattern <- "<!-- pkgdown-drop-below -->"
    idx <- grep(pattern, bfr)[1]
    if (!is.na(idx)) {
      bfr <- bfr[seq_len(idx-1)]
      writeLines(bfr, con = file)
    }
    stopifnot(file_test("-f", file))
  }

  ## Convert NEWS to NEWS.md?
  shim_news <- FALSE
  news_paths <- c("NEWS.md", "inst/NEWS.md")
  target_path <- file.path(build_path, news_paths)
  exists <- which(file_test("-f", target_path))
  if (length(exists) > 0) {
    news_path <- news_paths[exists]
    stopifnot(length(news_path) == 1)
    md <- prune_news_md(pkg, input = news_path, output = file.path(build_path, news_path))
    pruned <- attr(md, "pruned")
    if (pruned) {
      cat_line("Pruned NEWS.md file ", dst_path(news_path))
      stopifnot(file_test("-f", file.path(build_path, news_path)))
    }
  } else {
    news_path <- c("NEWS", "inst/NEWS")
    news_path <- news_path[file_test("-f", file.path(pkg, news_path))]
    if (length(news_path) >= 1) {
      news_path <- news_path[1]
      cat_line("Reading NEWS file ", src_path(news_path))
      news_to_md(pkg = pkg, input = news_path, output = file.path(build_path, "NEWS.md"), style = "pkgdown")
      cat_line("Writing NEWS.md file ", dst_path("NEWS.md"))
      stopifnot(file_test("-f", file.path(build_path, "NEWS.md")))
      shim_news <- TRUE
      attr(shim_news, "source") <- news_path
    }
  }

  pkgdown_path <- file.path(pkg, "pkgdown")
  if (file_test("-d", pkgdown_path)) {
    cat_line("Copying pkgdown folder ", src_path("pkgdown/"))
    file.copy(pkgdown_path, build_path, recursive = TRUE)
    target_path <- file.path(build_path, "pkgdown")
    stopifnot(file_test("-d", target_path))
  }

  opwd <- setwd(build_path)
  on.exit(setwd(opwd), add = TRUE)

  ## Shim vignettes
  vignettes <- pkgdown_shim_vignettes()

  pkgdown::build_site(pkg = ".", ..., preview = FALSE)
  docs_path <- "docs"
  stopifnot(file_test("-d", docs_path))

  ## GitHub specific files, e.g. CNAME
  if (github) {
    rule("Build GitHub-specific files", line = "=")
    build_github_pages()
  }

  rule("Postprocess package for pkgdown", line = "=")

  ## Fix up any references to the shim Rmarkdown files
  if (!is.null(vignettes)) {
    rule("Unshimming article sources")
    for (kk in seq_along(vignettes$docs)) {
      name <- vignettes$names[kk]
      shim_file <- basename(vignettes$shim_docs[kk])
      if (is.na(shim_file)) next  ## Not shimmed

      articles_path <- file.path("docs", "articles")
      article_file <- file.path(articles_path, sprintf("%s.html", name))
      cat_line("Updating ", dst_path(article_file))
      stopifnot(file_test("-d", articles_path))
      stopifnot(file_test("-f", article_file))
      content <- readLines(article_file)

      file <- basename(vignettes$docs[kk])
      
      ## Vignette source links (two parts)
      fmtstr <- "vignettes/%s"
      search <- sprintf(fmtstr, shim_file)
      replace <- sprintf(fmtstr, file)
      content <- gsub(search, replace, content, fixed = TRUE)
      
      fmtstr <- '<div class="hidden name"><code>%s</code></div>'
      search <- sprintf(fmtstr, shim_file)
      replace <- sprintf(fmtstr, file)
      content <- gsub(search, replace, content, fixed = TRUE)
      
      writeLines(content, con = article_file)
    }
  }

  if (shim_news) {
    rule("Unshimming ChangeLog source")
    news_path <- file.path("docs", "news")
    news_file <- file.path(news_path, "index.html")
    cat_line("Updating ", dst_path(news_file))
    stopifnot(file_test("-d", news_path), file_test("-f", news_file))
    content <- readLines(news_file)
    search <- sprintf("NEWS[.]md")
    replace <- attr(shim_news, "source")
    content <- gsub(search, replace, content)
    writeLines(content, con = news_file)
  }

  # Mention 'pkgdown.extras' in page footers
  htmls <- dir("docs", pattern = "[.]html$", full.names = TRUE, recursive = TRUE)
  search <- "<p>(Site built with .*)</p>"
  replace <- sprintf("<p>\\1 and <a href=\"https://github.com/HenrikBengtsson/%s/\">%s</a> %s.</p>", .packageName, .packageName, packageVersion(.packageName))
  warn <- TRUE
  for (kk in seq_along(htmls)) {
    html <- htmls[kk]
    bfr <- readLines(html, warn = FALSE)
    idxs <- grep(search, bfr)
    if (length(idxs) == 0) next
    bfr[idxs] <- gsub(search, replace, bfr[idxs])
    bfr[idxs] <- gsub(". and ", " and ", bfr[idxs], fixed = TRUE)
    bfr[idxs] <- gsub(".</p> and ", "</p> and ", bfr[idxs], fixed = TRUE)
    writeLines(bfr, con = html)
    warn <- FALSE
  }
  if (warn) {
    warning(sprintf("Failed to mention %s in the pkgdown page footer because we could not identify \"Site built with ...\" in any footer. Please report this to the %s maintainer. Thanks.", .packageName, .packageName))
  }

  # Update docsearch settings?
  docsearch <- config$template$params$docsearch
  if (length(docsearch) > 0) {
    if ("algoliaOptions" %in% names(docsearch)) {
      ## AD HOC: with read_yaml() -> as.yaml() we're losing critical yaml formatting
      ## Instead, bring in the raw string from the '_pkgdown.yml' file
      value <- grep("[[:space:]]*algoliaOptions:", readLines(pathname_yml), value = TRUE)
      ## Validate that we've go a single valid line
      expr <- tryCatch(read_yaml(text = value), error = identity)
      if (inherits(expr, "error")) {
        stop("The docsearch 'algoliaOptions' entry in the _pkgdown.yml file must be parsable and on a single line")
      }
      inject <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", value)
      stopifnot(length(inject) == 1L, is.character(inject), !is.na(inject), nzchar(inject))
      htmls <- dir("docs", pattern = "[.]html$", full.names = TRUE, recursive = TRUE)
      search <- "^(([[:space:]]*)indexName:[[:space:]]*'[[:alnum:].]+',)[[:space:]]*$"
      replace <- sprintf("\\1\n\\2%s,", inject)
      for (kk in seq_along(htmls)) {
        html <- htmls[kk]
        bfr <- readLines(html, warn = FALSE)
        idxs <- grep(search, bfr)
        if (length(idxs) == 0) next
        bfr[idxs] <- gsub(search, replace, bfr[idxs])
        writeLines(bfr, con = html)
      }
    } ## if (length(value) > 1)
  } ## if (length(docsearch) > 0)

  setwd(opwd)
  docs_path <- file.path(build_path, "docs")
  stopifnot(file_test("-d", docs_path))

  local({
    opwd <- setwd(pkg)
    on.exit(setwd(opwd))
    if (file_test("-d", "docs")) unlink("docs", recursive = TRUE)
    file.rename(docs_path, "docs")
    stopifnot(file_test("-d", "docs"))
  })
  
  preview_site(pkg = pkg, preview = preview)
}
