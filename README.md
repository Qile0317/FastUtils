# FastUtils

<!-- badges: start -->
[![R-CMD-check](https://github.com/Qile0317/FastUtils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Qile0317/FastUtils/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Qile0317/FastUtils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Qile0317/FastUtils?branch=main)
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Qile0317/FastUtils/blob/main/LICENSE.md)
<!-- badges: end -->

R package for fast, readable utility functions for general use. All functions have vectorized implementations whenever possible.

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

## Usage

```R
library(FastUtils)
```

There is a large collection of utility functions to use. Browse the reference manual with ```help(package = "FastUtils")``` to see all functions.

### Package Conventions

Almost all exported functions and parameters are named with `camelCase`, with the exception of those that try to modify or add on to existing functions from other packages with established ecosystems such as `testthat`.

## Contributing

Github pull requests from forked branches are more than welcome as it is mostly a solo-project at the moment. For major changes, please open an issue first to discuss what you would like to change. Please also make sure to update tests as appropriate.

## Contact

Qile Yang - qile.yang \[at\] berkeley.edu
