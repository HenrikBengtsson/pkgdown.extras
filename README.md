# pkgdown.extras - Enhancing the 'pkgdown' Package

The **[pkgdown]** package supports only Rmarkdown and knitr vignettes.  Vignettes using other types of vignette engines are not supported and are silently ignored by **pkgdown**. This package provides workarounds for some non-Rmarkdown vignette formats. For example, engine 'R.rsp::rsp' processes RSP-embedded Markdown vignettes into Markdown document, which then are compiled into final HTML documents. This package pre-compiles such documents into Markdown and tricks **pkgdown** to believe they are Rmarkdown vignettes so that they appear as **pkgdown** articles.


## Installation

To install this package, call:

```sh
remotes::install_github("HenrikBengtsson/pkgdown.extras")
```


## Disclaimer

I have no intention of submitting this to CRAN or providing end-user support
for it.  I use it for pkgdown:ing my own packages.


[pkgdown]: https://cran.r-project.org/packagee=pkgdown
