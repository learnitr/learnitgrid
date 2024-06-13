#' Create or read reference files for tests
#'
#' @description
#' Reference files allow to check if results are correct. They are not just a
#' copy of the result files. They are reencoded to avoid someone could just copy and paste from the reference to the results directories and cheat.
#'
#' @param name Name of the result file to transform into a reference file
#' @param ... Further parameters passed to [qs::qread()]
#' @param dir1 Directory containing the result file
#' @param dir2 Directory where to place the reference file
#' @param dir Directory containing the reference file
#' @param nthreads Number of threads to use for reading or writing the files
#'
#' @return The decoded content of the result file for `read_ref()` or the
#'   number of bytes written for `make_ref()`.
#' @export
make_ref <- function(name, ..., dir1 = here::here("tests", "results"),
    dir2 = here::here("tests", "reference"),
  nthreads = parallel::detectCores(logical = FALSE)) {
  res <- read_res(name, ..., dir = dir1, nthreads = nthreads)
  res <- qs::qserialize(res, preset = "archive")
  res <- qs::base85_encode(res)
  qs::qsave(res, file = fs::path(dir2, name), nthreads = nthreads, ...)
}

#' @export
#' @rdname make_ref
read_ref <- function(name, ..., dir = here::here("tests", "reference"),
    nthreads = parallel::detectCores(logical = FALSE)) {
  res <- qs::qread(fs::path(dir, name), nthreads = nthreads, ...)
  res <- qs::base85_decode(res)
  qs::qdeserialize(res)
}
