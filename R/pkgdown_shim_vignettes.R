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
      file_short <- file.path(basename(dirname(file)), basename(file))
      ext <- file_ext(file)
      engine_name <- vignettes$engines[kk]
      
      ## Already recognized by 'pkgdown'?
      ## Source: pkgdown:::package_vignettes
      if (grepl("\\.[rR]md$", ext)) {
        cat_line("Skipping ", src_path(file_short), "(processed by pkgdown)")
        next
      }
      
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
          cat_line("Weaving ", src_path(file_short))
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
          pkgdown_file_short <- file.path(basename(dirname(pkgdown_file)), basename(pkgdown_file))
          cat_line("Writing ", dst_path(pkgdown_file_short))
          file.rename(rmd, pkgdown_file)
          stopifnot(file_test("-f", pkgdown_file))

          shim_docs[kk] <- pkgdown_file
        }
      }

      if (is.na(shim_docs[kk])) {
        cat_line("Unsupported ", sQuote(engine_name), " format ", src_path(file_short))
      }
    } ## for (kk ...)
    
    setwd(opwd2)
    
    vignettes$shim_docs <- shim_docs
  } ## if (nvignettes > 0)

  vignettes
} ## pkgdown_shim_vignettes()
