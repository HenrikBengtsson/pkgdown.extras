comma <- function(x, sep = ", ") paste(x, collapse = sep)
commaq <- function(x, sep = ", ") paste(sQuote(x), collapse = sep)

import_from <- function(pkg, obj, mode = "function") {
  get(obj, mode = mode, envir = asNamespace(pkg))
}
