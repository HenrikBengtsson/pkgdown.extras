% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_pkgdown_yml.R
\name{build_pkgdown_yml}
\alias{build_pkgdown_yml}
\title{Build \verb{_pkgdown.yml} from \verb{_pkgdown.yml.rsp}}
\usage{
build_pkgdown_yml(
  pkg = ".",
  path = "pkgdown/_pkgdown.yml.rsp",
  validate = TRUE
)
}
\arguments{
\item{pkg}{Path to package.}

\item{path}{Relative path to \file{_pkgdown.yml.rsp}.}

\item{validate}{If TRUE, the YAML syntax of the
\file{_pkgdown.yml} file is validated.}
}
\value{
The path to \file{_pkgdown.yml}, or NULL.
}
\description{
Build \verb{_pkgdown.yml} from \verb{_pkgdown.yml.rsp}
}
\details{
This function compiles \file{_pkgdown.yml.rsp} into
\file{_pkgdown.yml}, if it exists.
}
