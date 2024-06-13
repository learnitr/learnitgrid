#' Test a project directory, possibly limit the number of uses
#'
#' @param path The path to test
#' @param reporter The {testthat} reporter to use
#' @param times The maximum number of times the tests can be run by the end-user
#' @param ... Additional arguments to pass to [testthat::test_dir()]
#'
#' @return The result of [testthat::test_dir()]
#' @export
test_dir <- function(path, reporter = project_reporter(),
    times = 10L, ...) {
  if (length(times) != 1 || !is.numeric(times) || times < 1L)
    stop("times must be a positive integer")
  if (fs::file_exists(".cnt")) {
    cnt <- try(readLines(".cnt") |> as.integer(), silent = TRUE)
    if (inherits(cnt, "try-error"))
      cnt <- times
  } else {
    cnt <- times
  }
  if (cnt < 1L) {
    cat("D\u00e9sol\u00e9, vous avez \u00e9puis\u00e9 vos ", times, " essais pour les tests !\n",
      sep = "")
    return(invisible())
  }
  # Decrement cnt, save and indicate how much is left
  cnt <- cnt - 1L
  writeLines(as.character(cnt), ".cnt")
  if (cnt == 0L) {
    cat("Attention : ceci est votre dernier essai pour les tests !\n",
      sep = "")
  } else {
    cat("Il vous reste ", cnt, " essais pour les tests apr\u00e8s celui-ci.\n",
      sep = "")
  }
  # Run the tests
  testthat::test_dir(path, reporter = reporter, ...)
}
