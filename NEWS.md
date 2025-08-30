# learnitgrid 0.9.2

-   Adaptation to {parsermd} v0.2.0: the `parse_rmd()` function lost its `allow_incomplete=` argument.

# learnitgrid 0.9.1

-   `here::here()` is replaced everywhere by `rprojroot::find_package_root_file()` because the former is not suitable any more in packages (it is for interactive use only).

# learnitgrid 0.9.0

-   Functions required to run projects and test that are added.

# learnitgrid 0.8.0

-   An extended example dataset is added (compressed in the package, but can be easily uncompressed using `install_example()`).

-    The `run_grid()` function is added to easily run the learnitgrid Shiny application.

-   Adding support for Quarto .qmd files.

-   New option docs_on_github to specify the location of the documents for the links. It is `TRUE` by default, which corresponds to previous behavior (i.e., links to GitHub files). If `FALSE`, the links will be to local files, except for "repo" and the subdirectory). 

# learnitgrid 0.7.1

-   Several package dependencies are eliminated.

# learnitgrid 0.7.0

-   Repo transferred to learnitr.

# learnitgrid 0.6.0

-   Renamed (from shiny_assess) and partly refactored.

-   Second tab by grid added.

-   Use a correction repository and check R objects (to be finished).

# learnitgrid 0.5.1

-   Allow for bonus points (max = 0 and score higher than max in this case).

# learnitgrid 0.5.0

-   Adjustment for teams + use of _corr.Rmd version.

# learnitgrid 0.4.0

-   Versions 0.1.0 - 0.4.0: initial concept and code.
