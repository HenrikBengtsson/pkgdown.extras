#' Build `_pkgdown.yml` from `_pkgdown.yml.rsp` 
#'
#' @details
#' This function compiles \file{_pkgdown.yml.rsp} into
#' \file{_pkgdown.yml}, if it exists.
#'
#' @param pkg Path to package.
#'
#' @param path Relative path to \file{_pkgdown.yml.rsp}.
#'
#' @param validate If TRUE, the YAML syntax of the
#' \file{_pkgdown.yml} file is validated.
#'
#' @returns The path to \file{_pkgdown.yml}, or NULL.
#'
#' @importFrom R.rsp rfile
#' @importFrom utils file_test
#' @importFrom tools file_path_sans_ext
#' @importFrom yaml read_yaml
#' @export
build_pkgdown_yml <- function(pkg = ".", path = "pkgdown/_pkgdown.yml.rsp", validate = TRUE) {
  rule <- import_from("pkgdown", "rule")
  cat_line <- import_from("pkgdown", "cat_line")
  src_path <- import_from("pkgdown", "src_path")
  dst_path <- import_from("pkgdown", "dst_path")
  
  stopifnot(file_test("-d", pkg))

  ## Nothing to do?
  pathname_rsp <- file.path(pkg, path)
  if (!file_test("-f", pathname_rsp)) return(NULL)

  cat_line("Compiling: ", src_path(path))
  local({
    dir <- dirname(pathname_rsp)
    file <- basename(pathname_rsp)
    opwd <- setwd(dir)
    on.exit(setwd(opwd), add = TRUE)
    rfile(file)
  })
  path_yml <- file_path_sans_ext(path)
  pathname_yml <- file.path(pkg, path_yml)
  stopifnot(file_test("-f", pathname_yml))
  
  if (validate) {
    cat_line("Validating: ", src_path(path_yml))
    yml <- read_yaml(pathname_yml)
  }
  
  invisible(pathname_yml)
}
