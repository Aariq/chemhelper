# ChemHelper
<!-- badges: start -->
  [![Project Status: Unsupported – The project has reached a stable, usable state but the author(s) have ceased all work on it. A new maintainer may be desired.](https://www.repostatus.org/badges/latest/unsupported.svg)](https://www.repostatus.org/#unsupported)
[![Travis build status](https://travis-ci.org/Aariq/chemhelper.svg?branch=master)](https://travis-ci.org/Aariq/chemhelper)[![codecov](https://codecov.io/gh/Aariq/chemhelper/branch/master/graph/badge.svg)](https://codecov.io/gh/Aariq/chemhelper)

  <!-- badges: end -->
I created this package as a place to house custom functions I've used a lot in analyses of analytical chemistry data durring my PhD.  Some of these functions are essentially **tidy** wrappers for functions from the `ropls` package that allow extraction of results from model objects into tidy data frames. the `*_IA()` functions deal with csv files output by Ion Analytics, an in-house deconvolution software.  Although it exports data as csv files, they are challenging due to line breaks within column headings, and a few other issues.  Other utilities include a version of `scale()` with many additional options for scaling, and a function to calculate retention indices for GC data given retention times and numbers of carbons of alkanes.  Although this package was primarily built with only me in mind, feel free to fork it or install it.

### Installation

You can install `chemhelper` from r-universe like so:

```r
# Install chemhelper in R:
install.packages('chemhelper', repos = c('https://aariq.r-universe.dev', 'https://cloud.r-project.org'))
```

Or you can install it directly from GitHub using the `remotes` package like so:

```r
library(remotes)
install_github("Aariq/chemhelper")
```
