#' @importFrom cli cat_line rule
#' @importFrom fs file_size
#' @importFrom utils file_test capture.output
#' @importFrom tools pkgVignettes vignetteEngine vignetteInfo file_path_sans_ext file_ext
#' @importFrom yaml write_yaml
pkgdown_shim_vignettes <- function(path = ".", ...) {
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
  assert_vignettes_recognized(vignettes, dir = ".")
  nvignettes <- length(vignettes$docs)
  if (nvignettes > 0) {
    rule("Shimming package vignettes")

    dir <- vignettes$dir
    stopifnot(file_test("-d", dir))
    opwd2 <- setwd(dir)

    old_options <- options()
    on.exit(options(old_options), add = TRUE)
    
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
      target_file <- file_path_sans_ext(file)
      target_ext <- file_ext(target_file)

      ## In case R options where changed by other vignettes
      options(old_options)

      ## Special cases
      if (engine_name == "R.rsp::rsp" && ext == "rsp") {
        ## *.md.rsp -> *.md
        if (target_ext == "md") {
          shim_docs[kk] <- Rmd_shim_md_rsp(file, yaml = yaml, engine = engine)
          next
        }
      }
      
      if (grepl("selfonly$", engine_name)) {
        ## *.md -> *.md
        if (ext == "md") {
          shim_docs[kk] <- Rmd_shim_selfonly(file, yaml = yaml, engine = engine)
          next
        }
      }

      shim_docs[kk] <- Rmd_shim_generic(file, yaml = yaml, engine = engine)
    } ## for (kk ...)
    
    setwd(opwd2)
    
    vignettes$shim_docs <- shim_docs
  } ## if (nvignettes > 0)

  vignettes
} ## pkgdown_shim_vignettes()


# Assert that all files declaring a %\VignetteEngine{...} directive were
# recognized by tools::pkgVignettes().  If the package that provides a
# vignette engine is not installed, then pkgVignettes() ignores such
# vignettes silently, which results in articles silently being dropped
# from the pkgdown site.
#' @importFrom utils file_test
#' @importFrom tools file_ext
assert_vignettes_recognized <- function(vignettes, dir = ".") {
  vign_dir <- vignettes$dir
  if (is.null(vign_dir)) vign_dir <- file.path(dir, "vignettes")
  if (!file_test("-d", vign_dir)) return(invisible(vignettes))

  files <- dir(vign_dir, full.names = TRUE)
  files <- files[file_test("-f", files)]

  ## Ignore non-source files, e.g. figures
  skip_exts <- c("css", "gif", "jpeg", "jpg", "pdf", "png", "svg", "webp")
  files <- files[!tolower(file_ext(files)) %in% skip_exts]
  if (length(files) == 0) return(invisible(vignettes))

  ## Which files declare a %\VignetteEngine{<pkg>::<engine>} directive?
  engines <- vapply(files, FUN.VALUE = NA_character_, FUN = function(file) {
    bfr <- tryCatch(suppressWarnings({
      readLines(file, n = 100L, warn = FALSE)
    }), error = function(e) character(0L))
    hit <- grep("\\VignetteEngine{", bfr, fixed = TRUE, value = TRUE,
                useBytes = TRUE)
    if (length(hit) == 0) return(NA_character_)
    sub(".*VignetteEngine[{]([^}]*)[}].*", "\\1", hit[1], useBytes = TRUE)
  })

  missed <- !is.na(engines) & !(basename(files) %in% basename(vignettes$docs))
  if (!any(missed)) return(invisible(vignettes))

  pkgs <- unique(sub("::.*", "", engines[missed]))
  installed <- vapply(pkgs, FUN.VALUE = FALSE, FUN = function(pkg) {
    nzchar(system.file(package = pkg))
  })

  missed_names <- sQuote(basename(files[missed]))
  if (length(missed_names) > 5) {
    missed_names <- c(missed_names[1:5],
                      sprintf("... (%d more)", length(missed_names) - 5L))
  }
  msg <- sprintf("Detected %d vignette file(s) that declare a vignette engine, but that were not recognized by tools::pkgVignettes(), meaning they would silently be dropped as pkgdown articles: %s.", sum(missed), paste(missed_names, collapse = ", "))
  if (any(!installed)) {
    msg <- sprintf("%s The most likely reason is that the package(s) providing the vignette engine(s) are not installed: %s. To fix this, install them and retry.", msg, paste(sQuote(pkgs[!installed]), collapse = ", "))
  } else {
    msg <- sprintf("%s The package(s) providing the vignette engine(s) appear to be installed (%s), so another possible reason is that the 'VignetteBuilder' field of the DESCRIPTION file does not list them.", msg, paste(sQuote(pkgs), collapse = ", "))
  }
  stop(msg, call. = FALSE)
}




# Turn an R.rsp::rsp *.md.rsp vignette into an Rmarkdown *.Rmd vignette
#' @importFrom cli cat_line
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom yaml write_yaml
Rmd_shim_md_rsp <- function(file, yaml, engine) {
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  
  target_dir <- dirname(file)
  target_file <- file_path_sans_ext(file)
  target_ext <- file_ext(target_file)
  file_short <- file.path(basename(dirname(file)), basename(file))

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

  ## 'pkgdown' will create a top ("H1") header with the title 'title'.
  ## It is likely that the *.md.rsp vignette already has such a top
  ## header with the same title.  That will result in duplicated
  ## titles at the top of the 'pkgdown' article.
  ## Drop top Markdown ("H1") header if it has the same title as the
  ## vignette (ignoring case).
  idx <- grep("^#[^#]", content)
  if (length(idx) > 0) {
    header <- sub("^#[[:space:]]+", "", content[idx[1]])
    header <- trim(header)
    if (tolower(header) == tolower(yaml$title)) {
      content <- content[-idx[1]]
    }
  }

  local({
    con <- file(rmd, open = "w+")
    on.exit(close(con))
    cat("---\n", file = con)
    write_yaml(yaml, file = con)
    cat("---\n", file = con)
    writeLines(content, con = con)
  })
  content <- NULL

  pkgdown_file <- file.path(target_dir, basename(rmd))
  stopifnot(!file_test("-f", pkgdown_file))
  pkgdown_file_short <- file.path(basename(dirname(pkgdown_file)), basename(pkgdown_file))
  cat_line("Writing ", dst_path(pkgdown_file_short))
  file.rename(rmd, pkgdown_file)
  stopifnot(file_test("-f", pkgdown_file))
  pkgdown_file
}


# Turn an *::selfonly *.md vignette into an Rmarkdown *.Rmd vignette
# Examples: startup::selfonly and progressr::selfonly
#' @importFrom cli cat_line
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom yaml write_yaml
Rmd_shim_selfonly <- function(file, yaml, engine) {
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  
  target_dir <- dirname(file)
  target_file <- file_path_sans_ext(file)
  target_ext <- file_ext(target_file)
  file_short <- file.path(basename(dirname(file)), basename(file))

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
  pkgdown_file
}


# Turn an vignette and its product into an Rmarkdown *.Rmd vignette
# Currently only PDFs are supported. HTML produces is on the to-do list.
#' @importFrom cli cat_line
#' @importFrom tools file_path_sans_ext file_ext texi2pdf
#' @importFrom yaml write_yaml
Rmd_shim_generic <- function(file, yaml, engine) {
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  find_vignette_product <- import_from("tools", "find_vignette_product")
  vignette_is_tex <- import_from("tools", "vignette_is_tex")
  
  target_dir <- dirname(file)
  target_file <- file_path_sans_ext(file)
  target_ext <- file_ext(target_file)
  file_short <- file.path(basename(dirname(file)), basename(file))
  name <- basename(sub(engine$pattern, "", file))

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

  ## Compile TeX?
  if (vignette_is_tex(target)) {
    texi2pdf(file = target, clean = FALSE, quiet = TRUE)
    target <- find_vignette_product(name, by = "texi2pdf", engine = engine, dir = ".")
  }

  ext <- tolower(file_ext(target))

  target_file <- file.path(target_dir, basename(target))
  file.rename(target, target_file)
  stopifnot(file_test("-f", target_file))

  if (ext == "html") {
    ## Create mockup Rmarkdown file
    rmd <- file.path(target_dir, paste(name, ".Rmd", sep = ""))

    content <- sprintf('<!--- <iframe src="%s"/> -->', basename(target_file))
    content <- c(content, sprintf('Vignette: [HTML](%s){target="_blank"} (%s)', basename(target_file), file_size(target_file)))
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
    return(pkgdown_file)
  }

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
    return(pkgdown_file)
  }
  
  cat_line("Unsupported output format ", sQuote(ext), " by vignette engine ", sQuote(paste0(engine$package, "::", engine$name)), " from ", src_path(file_short))

  NA_character_
}
