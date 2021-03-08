comma <- function(x, sep = ", ") paste(x, collapse = sep)
commaq <- function(x, sep = ", ") paste(sQuote(x), collapse = sep)
trim <- function(s) sub("[\t\n\f\r ]+$", "", sub("^[\t\n\f\r ]+", "", s))

import_from <- function(pkg, obj, mode = "function") {
  get(obj, mode = mode, envir = asNamespace(pkg))
}
