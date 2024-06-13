#' Create a testthat reporter suitable to test projects
#'
#' @return A testthat reporter
#' @export
project_reporter <- function() {
  # These variables are there to avoiding R CMD check to choke
  self <- NULL
  expectation_location <- NULL
  expectation_type <- NULL

  reporter <- testthat::LocationReporter

  reporter$public_methods$start_test <- function(context, test) {
    self$cat_line("  ", cli::symbol$bullet, " ", test)
  }

  reporter$public_methods$end_test <- function(context, test) {
    cli::cat_rule(width = 30L)
  }

  reporter$public_methods$add_result <- function(context, test, result) {
    status <- expectation_type(result)
    status_fr <- switch(status,
      success = "r\u00e9ussi",
      failure = "\u00e9chec",
      error = "erreur",
      skip = "ignor\u00e9",
      warning = "avis")
    if (status == "error" || status == "failure") {
      self$cat_line("    ", cli::col_red(cli::symbol$cross), " ",
        expectation_location(result), " [", status_fr, "]")
    } else if (status == "avis") {
      self$cat_line("    ", cli::col_yellow(cli::symbol$warning), " ",
        expectation_location(result), " [", status_fr, "]")
    } else if (status == "skip") {
      self$cat_line("    ", cli::col_cyan(cli::symbol$info), " ",
        expectation_location(result), " [", status_fr, "]")
    } else {# success
      self$cat_line("    ", cli::col_green(cli::symbol$tick), " ",
        expectation_location(result), " [", status_fr, "]")
    }
  }

  reporter$public_methods$start_file <- function(name) {
    name <- sub("test-([0-9]+)", "Fichier \\1 : ", name)
    name <- sub("\\.R$", "", name)
    name <- gsub("__", "/", name)
    name <- paste0("\n", cli::symbol$pointer, " ", name)
    self$cat_line(cli::col_cyan(name))
  }
  reporter
}
