#' @importFrom desc desc_get_field
#' @importFrom pkgbuild build
#' @importFrom utils file_test untar
#' @importFrom tools pkgVignettes vignetteEngine file_path_sans_ext file_ext
#' @importFrom yaml write_yaml
#' @export
pkgdown_build <- function(path = ".", ...) {
  rule <- import_from("pkgdown", "rule")
  cat_line <- import_from("pkgdown", "cat_line")
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  oopts <- options(width = 80L)
  on.exit(options(oopts))
  
  stopifnot(file_test("-d", path))

  pkgname <- desc_get_field("Package")

  rule("Preprocessing package for pkgdown", line = "=")

  build_root <- tempdir()
  build_path <- file.path(build_root, pkgname)
  
  cat_line("Create shim package folder", dst_path(build_path))
  tarball <- pkgbuild::build(
    path = ".",
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
  
  opwd <- setwd(build_path)
  on.exit(setwd(opwd), add = TRUE)

  ## Shim vignettes
  vignettes <- pkgdown_shim_vignettes()

  pkgdown::build_site(pkg = ".", preview = FALSE)
  docs_path <- "docs"
  stopifnot(file_test("-d", docs_path))


  rule("Postprocess package for pkgdown", line = "=")

  ## Fix up any references to the shim Rmarkdown files
  if (!is.null(vignettes)) {
    rule("Unshimming package articles")
    for (kk in seq_along(vignettes$docs)) {
      name <- vignettes$names[kk]
      file <- basename(vignettes$docs[kk])
      shim_file <- basename(vignettes$shim_docs[kk])
      article_file <- file.path("docs", "articles", sprintf("%s.html", name))
      cat_line("Updating ", dst_path(article_file))
      stopifnot(file_test("-f", article_file))
      content <- readLines(article_file)

      ## Vignette source links
      search <- sprintf("vignettes/%s", shim_file)
      replace <- sprintf("vignettes/%s", file)
      content <- gsub(search, replace, content)
      
      writeLines(content, con = article_file)
    }
  }


  setwd(opwd)
  docs_path <- file.path(build_path, "docs")
  stopifnot(file_test("-d", docs_path))

  if (file_test("-d", "docs")) unlink("docs", recursive = TRUE)
  file.rename(docs_path, "docs")
  stopifnot(file_test("-d", "docs"))
}




#' @importFrom utils file_test 
#' @importFrom tools pkgVignettes vignetteEngine file_path_sans_ext file_ext
#' @importFrom yaml write_yaml
pkgdown_shim_vignettes <- function(path = ".", ...) {
  rule <- import_from("pkgdown", "rule")
  cat_line <- import_from("pkgdown", "cat_line")
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")

  stopifnot(file_test("-d", path))
  opwd <- setwd(path)
  on.exit(setwd(opwd), add = TRUE)

  ## Vignettes
  vignettes <- pkgVignettes(dir = ".")
  nvignettes <- length(vignettes$docs)
  if (nvignettes > 0) {
    rule("Shimming package vignettes")

    dir <- vignettes$dir
    stopifnot(file_test("-d", dir))
    opwd2 <- setwd(dir)

    shim_docs <- rep(NA_character_, times = nvignettes)

    for (kk in seq_len(nvignettes)) {
      name <- vignettes$names[kk]
      file <- vignettes$docs[kk]
      cat_line("Weaving ", src_path(file))
      engine_name <- vignettes$engines[kk]
      engine <- vignetteEngine(engine_name)
      suppressPackageStartupMessages({
        if (!requireNamespace(engine$package, quietly = TRUE)) {
          stop("Failed to load vignette-builder package: ", sQuote(engine$package))
        }
      })

      ## Special cases
      if (engine_name == "R.rsp::rsp") {
        target_file <- file_path_sans_ext(file)
        target_dir <- dirname(target_file)
        target_ext <- file_ext(target_file)
        
        ## *.md.rsp
        if (target_ext == "md") {
          ## Compile *.md.rsp to *.md
          target <- local({
            oopts <- options(prompt = "> ", continue = "+ ")
            opwd3 <- setwd(tempdir())
            on.exit({
              setwd(opwd3)
              options(oopts)
            })
            suppressPackageStartupMessages({
              engine$weave(file, postprocess = FALSE, quiet = TRUE)
            })
          })
          
          # No longer needed
          file.remove(file)
          stopifnot(!file_test("-f", file))
          
          ## Create mockup Rmarkdown file
          rmd <- file_path_sans_ext(target)
          rmd <- paste(rmd, ".Rmd", sep = "")
          metadata <- attr(target, "metadata")
          content <- readLines(target)
          # No longer needed
          file.remove(target)
          stopifnot(!file_test("-f", target))
          
          yaml <- list(
            title  = metadata$title,
            author = metadata$author
          )

          local({
            con <- file(rmd, open = "w")
            on.exit(close(con))
            cat("---\n", file = con)
            write_yaml(yaml, file = con)
            cat("---\n", file = con)
            writeLines(content, con = con)
          })
          content <- yaml <- NULL

          pkgdown_file <- file.path(target_dir, basename(rmd))
          stopifnot(!file_test("-f", pkgdown_file))
          cat_line("Writing ", dst_path(pkgdown_file))
          file.rename(rmd, pkgdown_file)
          stopifnot(file_test("-f", pkgdown_file))

          shim_docs[kk] <- pkgdown_file
        }
      }
    } ## for (kk ...)
    
    setwd(opwd2)
    
    vignettes$shim_docs <- shim_docs
  } ## if (nvignettes > 0)

  vignettes
} ## pkgdown_shim_vignettes()
