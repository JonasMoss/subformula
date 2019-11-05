
<!-- README.md is generated from README.Rmd. Please edit that file -->

# subformula <img src="man/figures/logo.png" align="right" width="80" height="80" />

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/JonasMoss/subformula.svg?branch=master)](https://travis-ci.org/JonasMoss/subformula)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/JonasMoss/subformula?branch=master&svg=true)](https://ci.appveyor.com/project/JonasMoss/subformula)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/subformula)](https://cran.r-project.org/package=subformula)
[![Coverage
Status](https://codecov.io/gh/JonasMoss/subformula/branch/master/graph/badge.svg)](https://codecov.io/gh/JonasMoss/subformula?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

Subformula helps with automatic construction of the subformulas of an
`R` formula object.

This repository is work in progress, but should have a stable release
soon.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/subformula")
```

## Usage Example

``` r
library("subformula")
formula = y ~ x + u
as.list(subformulas(formula))
#> [[1]]
#> y ~ 1
#> 
#> [[2]]
#> y ~ x
#> 
#> [[3]]
#> y ~ u
#> 
#> [[4]]
#> y ~ x + u
```

## Documentation

A vignette coming soon\!

## How to Contribute or Get Help

If you encounter a bug, have a feature request or need some help, open a
[Github issue](https://github.com/JonasMoss/subformula/issues).This
project follows a [Contributor Code of Conduct](/CODE-OF-CONDUCT.md).
