# learnitgrid

<!-- badges: start -->

[![R-CMD-check](https://github.com/learnitr/learnitgrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/learnitr/learnitgrid/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://img.shields.io/codecov/c/github/learnitr/learnitgrid/main.svg)](https://codecov.io/github/learnitr/learnitgrid?branch=main)
[![CRAN Status](https://www.r-pkg.org/badges/version/learnitgrid)](https://cran.r-project.org/package=learnitgrid)
[![r-universe status](https://learnitr.r-universe.dev/badges/learnitgrid)](https://learnitr.r-universe.dev/learnitgrid)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

Marking work on the basis of marking grids is time-consuming and requires particular attention to ensure fair marking from one student to another.

learnitgrid, via a new web application {shiny}, makes it possible to mark all the work in a series criterion by criterion. The relevant part of each piece of work, identified by a title or a piece label, is extracted from the R/R Markdown/ Quarto documents. These extracts are grouped and sorted automatically using a text similarity calculation algorithm in the {stringdist} package. This makes it easier to score similar responses in the same way. This method also reduces the time needed for correction. Clickable links allow quick access to the complete job and to any context (dataset, additional documents, etc.), which further improves the speed of correction and the user’s comfort when filling in the criteria grids.

This package is associated with and enhances the {learnitdown} package.


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
