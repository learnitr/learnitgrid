#' A simple multiple choice system in a R chunk, compatible with git and R Markdown or Quarto documents
#'
#' @description
#' A simple multiple choice system in a R chunk, compatible with git and R
#' Markdown or Quarto documents. `obfuscate()` and `get_word()` are used to hide
#' correct answers in the {testthat} tests.
#'
#' @param x A String with the different answers, starting with []. The user has
#' to "check" the correct items by adding 'x' or 'X' inside the brackets
#' @param name The name of the select_answer. If not provided, the label of the
#' chunk where the `select_answer()` function is placed.
#'
#' @return A Markdown paragraph containing only the selected items (and the
#' selected items are also written in a result file)
#' @export
select_answer <- function(x, name = NULL) {
  ans <- strsplit(x, "\n[", fixed = TRUE)[[1]][-1]
  # Keep only ckecked answers
  checked <- grepl("^[xX]\\]", ans)
  ans <- ans[checked]
  # Remove check marks
  ans <- sub("^[xX]\\] ? ?", "", ans)
  # Print result
  cat(ans, "", sep = "\n")
  # Save a digest of the results in a variable in \tests\results
  # Note: the chunk label is only available on the document
  # rendering, not when the chunk is executed in the R console!
  # So, if we rely on it, nothing is saved unless the document is
  # knitted!
  if (is.null(name))
    name <- knitr::opts_current$get('label')
  if (!is.null(name)) {
    dir.create(here::here("tests", "results"), showWarnings = FALSE)
    res <- digest::digest(ans)
    # In case we are in correction mode, output more info
    if (getOption("learnitdown.correction", default = FALSE)) {
      # Second line is 1/0 for each option (checked or not)
      res <- c(res, paste(as.integer(checked), collapse = " "))
      # Finally add the whole text
      res <- c(res, x)
    }
    write_res(res, name)
  }
  invisible(ans)
}

#' @export
#' @rdname select_answer
obfuscate <- function(x) {
  stopifnot(length(x) == 1, is.character(x))
  qs::base91_encode(charToRaw(x))
}

#' @export
#' @rdname select_answer
get_word <- function(x) {
  stopifnot(length(x) == 1, is.character(x))
  rawToChar(qs::base91_decode(x))
}
