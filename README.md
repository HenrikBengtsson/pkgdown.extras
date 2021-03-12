# pkgdown.extras - Enhancing the 'pkgdown' Package

The **[pkgdown]** package supports only Rmarkdown and knitr vignettes.  Vignettes using other types of vignette engines are not supported and are silently ignored by **pkgdown**. This package provides workarounds for some non-Rmarkdown vignette formats. For example, engine 'R.rsp::rsp' processes RSP-embedded Markdown vignettes into Markdown document, which then are compiled into final HTML documents. This package pre-compiles such documents into Markdown and tricks **pkgdown** to believe they are Rmarkdown vignettes so that they appear as **pkgdown** articles.


## Usage

### Build full pkgdown site

From R:

```sh
> pkgdown.extras::build_site()
```

From the command line:

```sh
Rscript -e pkgdown.extras::build_site
```


This will build the full pkgdown website in a temporary folder and copy the results to the `docs/` folder in the current working directory.  When building the website, the above function will:

1. "shim" any `NEWS` (or `inst/NEWS`) file to a `NEWS.md` file that is recognized by **pkgdown**.  The result is that the NEWS entries are rendered nicely in the 'ChangeLog' menu created by **pkgdown**.  (Function `news_to_md()` can be used to manually generate a `NEWS.md` file from a `NEWS` file)

2. shim non-Rmarkdown vignettes, which are silently ignored by **pkgdown**, into Rmarkdown vignettes.  This is achieved by weaving the vignette using the vignette engine that the vignette declares, which results in either a HTML or a PDF vignette product.  For PDFs, a shim Rmarkdown vignette, which will both provide a link to the PDF as well as including it in an `<iframe>`, is created on-the-fly so **pkgdown** incorporates the vignette.  HTML documents are automatically added to the 'Articles' menu by **pkgdown**; when clicking on those in the menu, the vignette will be opened in a separate browser tab.

For all shimmed documents, any "Source: &lt;link&gt;" is adjusted such that it links to the proper source.


## Installation

To install this package, call:

```sh
remotes::install_github("HenrikBengtsson/pkgdown.extras")
```


## Disclaimer

I have no intention of submitting this to CRAN or providing end-user support
for it.  I use it for pkgdown:ing my own packages.


[pkgdown]: https://cran.r-project.org/packagee=pkgdown
