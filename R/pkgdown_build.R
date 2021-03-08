#' Build a complete 'pkgdown' website
#'
#' @param pkg Path to package.
#'
#' @param \ldots ... Additional arguments passed to [pkgdown::build_site()].
#'
#' @param preview Preview site in browser?
#'
#' @returns Nothing.
#'
#' @importFrom desc desc_get_field
#' @importFrom pkgbuild build
#' @importFrom utils file_test untar
#' @importFrom tools pkgVignettes vignetteEngine file_path_sans_ext file_ext
#' @importFrom yaml write_yaml
#' @export
pkgdown_build_site <- function(pkg = ".", ..., preview = NA) {
  rule <- import_from("pkgdown", "rule")
  cat_line <- import_from("pkgdown", "cat_line")
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  preview_site <- import_from("pkgdown", "preview_site")
  oopts <- options(width = 80L)
  on.exit(options(oopts))
  
  stopifnot(file_test("-d", pkg))

  pkgname <- desc_get_field("Package")

  rule("Preprocessing package for pkgdown", line = "=")

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


  rule("Postprocess package for pkgdown", line = "=")

  ## Fix up any references to the shim Rmarkdown files
  if (!is.null(vignettes)) {
    rule("Unshimming package articles")
    for (kk in seq_along(vignettes$docs)) {
      name <- vignettes$names[kk]
      shim_file <- basename(vignettes$shim_docs[kk])
      if (is.na(shim_file)) next  ## Not shimmed
      
      article_file <- file.path("docs", "articles", sprintf("%s.html", name))
      cat_line("Updating ", dst_path(article_file))
      stopifnot(file_test("-f", article_file))
      content <- readLines(article_file)

      ## Vignette source links
      file <- basename(vignettes$docs[kk])
      search <- sprintf("vignettes/%s", shim_file)
      replace <- sprintf("vignettes/%s", file)
      content <- gsub(search, replace, content)
      
      writeLines(content, con = article_file)
    }
  }

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
