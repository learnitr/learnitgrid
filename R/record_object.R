#' Record, read and write results that capture given characteristics of an object
#'
#' @param object The object to record results from (`".Last.chunk"` by default,
#'   which corresponds to the last R object produced in the current chunk in an
#'   R Markdown or Quarto document)
#' @param name The name of the result file
#' @param fun The function used to compute results for the object
#' @param ... Additional arguments to pass to `fun=` or to the read/write
#'   functions using [qs::qsave()] and [qs::qread()]
#' @param dir The directory where to save the results
#' @param env The environment where to look for the object
#' @param nthreads The number of threads to use for reading and writing
#'
#' @details
#' The main function to record the results is `record_res()`. However, there are
#' several shortcuts to record specific characteristics of an object: `RO` is
#' the same as `record_res()`, `RN()` records by default `".Last.chunk"` but
#' focuses on the name of the result file, `RODFS()` records the main structure
#' of a data frame, `RNDFS()` is the same as `RODFS()` but focuses on the name
#' of the result file, `ROMD5()` records the MD5 hash of an object, `RNMD5()` is
#' the same focusing on the name, `ROP()` records one or several parts of an
#' object (items from a list), `RNP()` is the same focusing on the name, `ROA()`
#' records one or several attributes of the objects with `RNA()` focusing on the
#' name, `ROSTR()` records the [utils::str()] summary of the object, and
#' `RNSTR()` is the same focusing on the name of the result file.
#'
#' @return The deserialized results for `read_res()` or the number of bytes
#'   written invisibly for `write_res()`. `record_res()` invisibly returns
#'   `TRUE` or `FALSE` depending on the success of the operation.
#' @export
# The main function to put a result in /tests/results
record_res <- function(object = ".Last.chunk", name = object,
    fun = NULL, ..., dir = here::here("tests", "results"),
    env = parent.frame()) {
  data <- get0(object, envir = env)
  if (is.null(data))
    return(invisible(FALSE))

  if (!is.null(fun))
    data <- try(fun(data, ...), silent = TRUE)

  write_res(data, name = name, dir = dir)
  invisible(TRUE)
}

#' @export
#' @rdname record_res
read_res <- function(name, ..., dir = here::here("tests", "results"),
    nthreads = parallel::detectCores(logical = FALSE)) {
  qs::qread(fs::path(dir, name), nthreads = nthreads, ...)
}

#' @export
#' @rdname record_res
write_res <- function(object, name, ..., dir = here::here("tests", "results"),
    nthreads = parallel::detectCores(logical = FALSE)) {
  fs::dir_create(dir)
  qs::qsave(object, file = fs::path(dir, name), nthreads = nthreads, ...)
}

# Shortcuts

#' @export
#' @rdname record_res
RO <- record_res

#' @export
#' @rdname record_res
RN <- function(name, object = ".Last.chunk", fun = NULL, ...,
  env = parent.frame())
  record_res(object = object, name = name, fun = fun, ..., env = env)

#' @export
#' @rdname record_res
RODFS <- function(object = ".Last.chunk", name = object,
  fun = df_structure, ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, ..., env = env)

#' @export
#' @rdname record_res
RNDFS <- function(name, object = ".Last.chunk", fun = df_structure, ...,
  env = parent.frame())
  record_res(object = object, name = name, fun = fun, ..., env = env)

#' @export
#' @rdname record_res
ROMD5 <- function(object = ".Last.chunk", name = object, fun = digest,
  ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, ..., env = env)

#' @export
#' @rdname record_res
RNMD5 <- function(name, object = ".Last.chunk", fun = digest, ...,
  env = parent.frame())
  record_res(object = object, name = name, fun = fun, ..., env = env)

#' @export
#' @rdname record_res
#' @param part The part(s) (list item(s)) to use
ROP <- function(object = ".Last.chunk", part = "x", name = object,
  fun = object_part, ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, part = part,
    ..., env = env)

#' @export
#' @rdname record_res
RNP <- function(name, part = "x", object = ".Last.chunk",
  fun = object_part, ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, part = part,
    ..., env = env)

#' @export
#' @rdname record_res
ROA <- function(object = ".Last.chunk", attrib = "class",
  name = object, fun = object_attr, ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, attrib = attrib,
    ..., env = env)

#' @export
#' @rdname record_res
#' @param attrib The attribute(s) to use
RNA <- function(name, attrib = "class", object = ".Last.chunk",
  fun = object_attr, ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, attrib = attrib,
    ..., env = env)

#' @export
#' @rdname record_res
ROSTR <- function(object = ".Last.chunk", part = "x", name = object,
  fun = object_str, ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, part = part,
    ..., env = env)

#' @export
#' @rdname record_res
RNSTR <- function(name, part = "x", object = ".Last.chunk",
  fun = object_str, ..., env = parent.frame())
  record_res(object = object, name = name, fun = fun, part = part,
    ..., env = env)
