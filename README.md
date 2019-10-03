
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SCtools

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/medewitt/SCtools.svg?branch=master)](https://travis-ci.org/medewitt/SCtools)
[![Codecov test
coverage](https://codecov.io/gh/medewitt/SCtools/branch/master/graph/badge.svg)](https://codecov.io/gh/medewitt/SCtools?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/SCtools)](https://CRAN.R-project.org/package=SCtools)
<!-- badges: end -->

This package is a work in progress compiling functions to make it easier
running placebo in space tests with synthetic control models and SCM
with multiple treated units. It relies heavily on the {Synth} package.

You can install it on CRAN with:

``` r
install.packages("SCtools")
```

You can install it with {devtools}.

``` r
library(devtools)

install_github("bcastanho/SCtools")
```

Or with the {remotes} package

``` r
library(remotes)
install_github("bcastanho/SCtools")
```

Please note that the ‘SCtools’ project is released with a [Contributor
Code of
Conduct](https://github.com/bcastanho/SCtools/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
