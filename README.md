# FastUtils

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/FastUtils)](https://CRAN.R-project.org/package=FastUtils)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/FastUtils?color=brightgreen)](https://www.r-pkg.org/pkg/FastUtils)
[![R-CMD-check](https://github.com/Qile0317/FastUtils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Qile0317/FastUtils/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Qile0317/FastUtils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Qile0317/FastUtils?branch=main)
[![Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](https://qile0317.github.io/APackOfTheClones/)
[![Developmental Documentation](https://img.shields.io/badge/docs-dev-blue.svg)](https://qile0317.github.io/APackOfTheClones/dev/)
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Qile0317/FastUtils/blob/main/LICENSE.md)
<!-- badges: end -->

A wide variety of tools for general data analysis, wrangling, spelling, statistics, visualizations, package development, and more. All functions have vectorized implementations whenever possible. Exported names are designed to be readable, with longer names possessing short aliases.

## Installation

FastUtils is registered on CRAN. To install, try the following

```R
install.packages("FastUtils")
```

For the latest development edition, try the following

```R
library(devtools)
devtools::install_github("Qile0317/FastUtils")
```

## Usage and Documentation

```R
library(FastUtils)
```

There is a large collection of utility functions to use. Browse the reference manual with ```help(package = "FastUtils")``` to see all functions. Alternatively, check the reference manual online here: <https://qile0317.github.io/FastUtils/reference/index.html>.

### Package Conventions

Almost all exported functions and parameters are named with `camelCase`, with the exception of those that try to modify or add on to existing functions from other packages with established ecosystems such as `testthat`.

## Contributing

Github pull requests from forked branches are more than welcome as it is mostly a solo-project at the moment. For major changes, please open an issue first to discuss what you would like to change. Please also make sure to update tests as appropriate. See `CONTRIBUTING.md` for more information.

## Contact

Qile Yang - qile.yang \[at\] berkeley.edu
