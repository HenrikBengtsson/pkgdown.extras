#' @importFrom fs file_size
#' @importFrom utils file_test capture.output
#' @importFrom tools pkgVignettes vignetteEngine vignetteInfo file_path_sans_ext file_ext
#' @importFrom yaml write_yaml
pkgdown_shim_vignettes <- function(path = ".", ...) {
  rule <- import_from("pkgdown", "rule")
  cat_line <- import_from("pkgdown", "cat_line")
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  get_vignette_metadata <- import_from("tools", ".get_vignette_metadata")
  find_vignette_product <- import_from("tools", "find_vignette_product")

  stopifnot(file_test("-d", path))
  opwd <- setwd(path)
  on.exit(setwd(opwd), add = TRUE)

  ## AD HOC: Fake 'R CMD build'
  ## (some packages register vignette engines only when that is running)
  rcmd <- Sys.getenv("R_CMD", NA_character_)
  on.exit({
    if (is.na(rcmd)) Sys.unsetenv("R_CMD") else Sys.setenv("R_CMD" = rcmd)
  }, add = TRUE)
  Sys.setenv("R_CMD" = "true")
  
  ## Find vignettes
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
      
      vinfo <- vignetteInfo(file)
      vinfo$author <- get_vignette_metadata(readLines(file), "Author")

      engine <- vignetteEngine(vinfo$engine)
      suppressPackageStartupMessages({
        if (!requireNamespace(engine$package, quietly = TRUE)) {
          stop("Failed to load vignette-builder package: ", sQuote(engine$package))
        }
      })

      yaml <- list(
        title  = vinfo$title,
        author = vinfo$author
      )
      
      target_dir <- dirname(file)

      ## Special cases
      if (engine_name == "R.rsp::rsp") {
        target_file <- file_path_sans_ext(file)
        target_ext <- file_ext(target_file)

        ## *.md.rsp -> *.md
        if (ext == "rsp" && target_ext == "md") {
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
          content <- readLines(target)
          # No longer needed
          file.remove(target)
          stopifnot(!file_test("-f", target))
          local({
            con <- file(rmd, open = "w+")
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
          next
        }
      }
      
      if (grepl("selfonly$", engine_name)) {
        ## *.md -> *.md
        if (ext == "md") {
          cat_line("Shimming ", src_path(file_short))
          ## Create mockup Rmarkdown file
          rmd <- file_path_sans_ext(file)
          rmd <- paste(rmd, ".Rmd", sep = "")
          content <- readLines(file)
          local({
            con <- file(rmd, open = "w+")
            on.exit(close(con))
            cat("---\n", file = con)
            write_yaml(yaml, file = con)
            cat("---\n", file = con)
            writeLines(content, con = con)
          })
          content <- yaml <- NULL

          pkgdown_file <- file.path(target_dir, basename(rmd))
          pkgdown_file_short <- file.path(basename(dirname(pkgdown_file)), basename(pkgdown_file))
          cat_line("Writing ", dst_path(pkgdown_file_short))
          stopifnot(file_test("-f", pkgdown_file))

          shim_docs[kk] <- pkgdown_file
          next
        }
      }
      
      cat_line("Weaving ", src_path(file_short))
      target <- local({
        oopts <- options(prompt = "> ", continue = "+ ")
        opwd3 <- setwd(dirname(file))
        on.exit({
          setwd(opwd3)
          options(oopts)
        })
        capture.output(suppressMessages(suppressPackageStartupMessages({
          engine$weave(file, quiet = TRUE)
        })))
        file.path(getwd(), find_vignette_product(name, by = "weave", engine = engine, dir = "."))
      })
      stopifnot(file_test("-f", target))
      ext <- tolower(file_ext(target))

      target_file <- file.path(target_dir, basename(target))
      file.rename(target, target_file)
      stopifnot(file_test("-f", target_file))

      if (ext == "pdf") {
        ## Create mockup Rmarkdown file
        rmd <- file.path(target_dir, paste(name, ".Rmd", sep = ""))

        content <- sprintf('<!--- <iframe src="%s"/> -->', basename(target_file))
        content <- c(content, sprintf('Vignette: [PDF](%s){target="_blank"} (%s)', basename(target_file), file_size(target_file)))
        content <- c(content, sprintf('<iframe src="%s" width="100%%" height="1000"/>', basename(target_file)))
        
        local({
          con <- file(rmd, open = "w+")
          on.exit(close(con))
          cat("---\n", file = con)
          write_yaml(yaml, file = con)
          cat("---\n", file = con)
          writeLines(content, con = con)
        })
        content <- yaml <- NULL
        
        pkgdown_file <- file.path(target_dir, basename(rmd))
        pkgdown_file_short <- file.path(basename(dirname(pkgdown_file)), basename(pkgdown_file))
        cat_line("Writing ", dst_path(pkgdown_file_short))
        stopifnot(file_test("-f", pkgdown_file))

        shim_docs[kk] <- pkgdown_file
        next
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
