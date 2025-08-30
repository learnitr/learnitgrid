#' Transformation functions for creating reference or result objects for testing
#' progress of the students in their project.
#'
#' @param object The object to transform
#' @param ... Further arguments (not used for now)
#'
#' @return `df_structure()` returns names, attributes "label" and "units",
#' number of row and columns, classes, if there are missing data and comment in
#' a data frame. `digest()` returns a hash of the object. `object_attr()`
#' returns the attribute(s) of the object. `object_part()` returns the part(s)
#' of an object (list items). `object_str()` returns a str() representation of
#' the object parts.
#' @export
#'
#' @examples
#' # not yet
df_structure <- function(object, ...) {
  list(
    names = if (is.matrix(object)) colnames(object) else names(object),
    labels = if (is.matrix(object)) NULL else lapply(object, function(x) {
      res <- attr(x, "label")
      if (is.null(res) || is.na(res)) "" else as.character(res)
    }),
    units = if (is.matrix(object)) NULL else lapply(object, function(x) {
      res <- attr(x, "units")
      if (is.null(res) || is.na(res)) "" else as.character(res)
    }),
    nrow = nrow(object),
    ncol = ncol(object),
    classes = if (is.matrix(object)) class(object) else
      sapply(object, function(x) class(x)[1]),
    nas = if (is.matrix(object)) sum(is.na(object)) else
      sapply(object, function(x) sum(is.na(x))),
    comment = comment(object)
  )
}

#' @export
#' @rdname df_structure
#' @param algo algorithm to use for digest, `"md5"` by default
digest <- function(object, algo = "md5", ...) {
  # Remove spec and problems attributes and convert to a data.frame if needed
  attr(object, "spec") <- NULL
  attr(object, "problems") <- NULL
  if (inherits(object, "data.frame"))
    object <- as.data.frame(object)
  digest::digest(object, algo = algo, ...)
}

#' @export
#' @rdname df_structure
#' @param attrib The attribute(s) to record from the object
object_attr <- function(object, attrib = "class", ...) {
  # Only record one or more attributes of an object
  attribs <- strsplit(attrib, ",", fixed = TRUE)[[1]] |> trimws()
  all_attribs <- attributes(object)
  all_attribs[attribs]
}

#' @export
#' @rdname df_structure
#' @param part The part(s) to record from the object (list items)
object_part <- function(object, part = "x", ...) {
  # Only record one or more parts of an object
  parts <- strsplit(part, ",", fixed = TRUE)[[1]] |> trimws()
  if (inherits(object, "data.frame"))
    object <- as.data.frame(object)
  object[parts]
}

#' @export
#' @rdname df_structure
object_str <- function(object, part = "x", ...) {
  res <- object_part(object, part = part)
  str(res, ...) |> utils::capture.output()
}
