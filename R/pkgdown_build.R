#' @importFrom desc desc_get_field
#' @importFrom pkgbuild build
#' @importFrom utils file_test
#' @importFrom tools pkgVignettes vignetteEngine
#' @export
pkgdown_build <- function(path = ".", ...) {
  stopifnot(file_test("-d", path))

  pkgname <- desc::desc_get_field("Package")

  build_root <- tempdir()
  build_path <- file.path(build_root, pkgname)

  tarball <- pkgbuild::build(
    path = ".",
    dest_path = tempdir(),
    vignettes = FALSE,
    args = c("--no-resave-data", "--no-manual")
  )

  ## Create intermediate package folder
  if (file_test("-d", build_path)) unlink(build_path, recursive = TRUE)
  stopifnot(!file_test("-d", build_path))
  res <- untar(tarball, exdir = tempdir())
  stopifnot(res == 0)
  stopifnot(file_test("-d", build_path))
  
  opwd <- setwd(build_path)
  on.exit(setwd(opwd), add = TRUE)

  ## Vignettes
  vignettes <- tools::pkgVignettes(dir = ".")
  nvignettes <- length(vignettes$docs)
  message("Number of vignettes: ", nvignettes)
  if (nvignettes > 0) {
    message(sprintf("Processing vignettes: [n=%d] %s",
            nvignettes, commaq(vignettes$names)))
    vignette_builders <- desc::desc_get_field("VignetteBuilder")
    for (pkg in vignette_builders) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("Failed to load vignette-builder package: ", sQuote(pkg))
      }
    }

    dir <- vignettes$dir
    stopifnot(file_test("-d", dir))
    opwd2 <- setwd(dir)

    for (kk in seq_len(nvignettes)) {
      name <- vignettes$names[kk]
      file <- vignettes$docs[kk]
      message(sprintf("Vignette %d (%s) of %d", kk, name, length(vignettes)))
      engine_name <- vignettes$engine[kk]
      engine <- tools::vignetteEngine(engine_name)
      if (!requireNamespace(engine$package, quietly = TRUE)) {
        stop("Failed to load vignette-builder package: ", sQuote(engine$package))
      }

      ## Special cases
      if (engine_name == "R.rsp::rsp") {
        target_file <- tools::file_path_sans_ext(file)
        target_dir <- dirname(target_file)
        target_ext <- tools::file_ext(target_file)
        
        ## *.md.rsp
        if (target_ext == "md") {
          ## Compile *.md.rsp to *.md
          opwd3 <- setwd(tempdir())
          target <- engine$weave(file, postprocess = FALSE)
          setwd(opwd3)
          
          ## Create mockup Rmarkdown file
          rmd <- tools::file_path_sans_ext(target)
          rmd <- paste(rmd, ".Rmd", sep = "")
          metadata <- attr(target, "metadata")
          yaml <- list(
            title  = metadata$title,
            author = metadata$author
          )
          bfr <- readLines(target)
          con <- file(rmd, open = "w")
          cat("---\n", file = con)
          yaml::write_yaml(yaml, file = con)
          cat("---\n", file = con)
          writeLines(bfr, con = con)
          close(con)

          # No longer needed
          file.remove(target_file)
          stopifnot(!file_test("-f", target_file))

          pkgdown_file <- file.path(target_dir, basename(rmd))
          stopifnot(!file_test("-f", pkgdown_file))
          file.rename(rmd, pkgdown_file)
          stopifnot(file_test("-f", pkgdown_file))
        }
      }
    } ## for (kk ...)
    
    setwd(opwd2)
  } ## if (nvignettes > 0)

  pkgdown::build_site(pkg = ".", preview = FALSE)
  docs_path <- "docs"
  stopifnot(file_test("-d", docs_path))
  setwd(opwd)

  docs_path <- file.path(build_path, "docs")
  stopifnot(file_test("-d", docs_path))

  if (file_test("-d", "docs")) unlink("docs", recursive = TRUE)
  file.rename(docs_path, "docs")
  stopifnot(file_test("-d", "docs"))
}
