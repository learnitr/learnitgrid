# learnitgrid

<!-- badges: start -->

[![R-CMD-check](https://github.com/learnitr/learnitgrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/learnitr/learnitgrid/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://img.shields.io/codecov/c/github/learnitr/learnitgrid/main.svg)](https://codecov.io/github/learnitr/learnitgrid?branch=main)
[![CRAN Status](https://www.r-pkg.org/badges/version/learnitgrid)](https://cran.r-project.org/package=learnitgrid)
[![r-universe status](https://learnitr.r-universe.dev/badges/learnitgrid)](https://learnitr.r-universe.dev/learnitgrid)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

Marking work on the basis of rubrics (evaluation grids) is time-consuming and requires particular attention to ensure fair marking from one student to another. {learnitgrid} is a Shiny application that makes it possible to evaluate students's work in R scripts, R Markdown or Quarto documents more easily and more rapidly.

## Installation

You can install the development version of {learnitgrid} from [GitHub](https://github.com/) with:

``` r
#install.packages("remotes")
remotes::install_github("learnitr/learnitgrid")
```

## Example

An example dataset with anonymized data providing from three assignments is included in the package. The dataset is compressed, but it can be easily uncompressed using `install_grid_example()`. Then, `run_grid()` starts the learnitgrid Shiny application.

``` r
library(learnitgrid)
install_grid_example()
run_grid()
```

# Code of Conduct

Please note that the {learnitgrid} package is released with a Contributor Code of Conduct. By contributing to this project, you agree to abide by its terms.
