#' Functions to be used in testthat test of the project
#'
#' @param name The name of the result and reference files
#' @param part The part(s) (list item(s)) to co=pare
#' @param attr  The attribute(s) to compare
#'
#' @return `TRUE` if the result is identical to the reference, `FALSE` otherwise
#' @export
is_identical_to_ref <- function(name, part = NULL, attr = NULL) {
  ref <- read_ref(name) # Note: generate an error if the object does not exist
  res <- read_res(name) # Idem

  if (!is.null(part)) {
    ref <- ref[[part]]
    res <- res[[part]]
  }

  if (!is.null(attr)) {
    ref <- attr(ref, attr)
    res <- attr(res, attr)
  }

  # Items cannot be NULL
  if (is.null(res) && is.null(ref))
    structure(FALSE, message = "Both res and ref are NULL")

  identical(res, ref)
}

#' @export
#' @rdname is_identical_to_ref
is_equal_to_ref <- function(name, part = NULL, attr = NULL) {
  ref <- read_ref(name) # Note: generate an error if the object does not exist
  res <- read_res(name) # Idem

  if (!is.null(part)) {
    ref <- ref[[part]]
    res <- res[[part]]
  }

  if (!is.null(attr)) {
    ref <- attr(ref, attr)
    res <- attr(res, attr)
  }

  # Items cannot be NULL
  if (is.null(res) && is.null(ref))
    structure(FALSE, message = "Both res and ref are NULL")

  all.equal(res, ref)
}

# Check if the rendered version of a Quarto or R Markdown file exists
#' @export
#' @rdname is_identical_to_ref
#' @param quarto The name of the Quarto or R Markdown file
#' @param format The format of the rendered file. For `is_rendered()` or `is_rendered_current()` it is `"html'` by default. For the other function, the default is `"rds"`
is_rendered <- function(quarto, format = "html") {
  rendered <- sub("\\.[qR]md$", paste0(".", format), quarto)
  rendered_path <- here::here(rendered)
  fs::file_exists(rendered_path)
}

# Check if the rendered version is up-to-date
#' @export
#' @rdname is_identical_to_ref
is_rendered_current <- function(quarto, format = "html") {
  rendered <- sub("\\.[qR]md$", paste0(".", format), quarto)
  quarto_path <- here::here(quarto)
  rendered_path <- here::here(rendered)
  fs::file_exists(rendered_path) &&
    file.mtime(rendered_path) >= file.mtime(quarto_path)
}

# A data file exists and contains a data.frame
#' @export
#' @rdname is_identical_to_ref
#' @param dir The subdirectory in the package where to look for (`"data"` by default)
#' @param check_df Check if the data file contains a data frame
is_data <- function(name, dir = "data", format = "rds", check_df = FALSE) {
  data_path <- here::here(dir, paste(name, format, sep = "."))
  res <- fs::file_exists(data_path)
  if (!res)
    return(structure(FALSE, message = paste0("The data file ", data_path,
      " is not found.")))
  res <- try(data.io::read(data_path, type = format), silent = TRUE)
  if (inherits(res, "try-error"))
    return(structure(FALSE, message = res))

  if (isTRUE(check_df) && !inherits(res, "data.frame"))
    return(structure(FALSE, message = paste0("The data file ", data_path,
      " is found but it does not contains a data frame.")))

  # Everything is OK
  TRUE
}

#' @export
#' @rdname is_identical_to_ref
is_data_df <- function(name, dir = "data", format = "rds", check_df = TRUE)
  is_data(name, dir = dir, format = format, check_df = check_df)

#' @export
#' @rdname is_identical_to_ref
has_labels_all <- function(name, part = NULL) {
  res <- read_res(name)$labels
  res <- sapply(res, nchar) > 0
  all(res, na.rm = TRUE)
}

#' @export
#' @rdname is_identical_to_ref
has_labels_any <- function(name, part = NULL) {
  res <- read_res(name)$labels
  res <- sapply(res, nchar) > 0
  any(res, na.rm = TRUE)
}

#' @export
#' @rdname is_identical_to_ref
has_units_all <- function(name, part = NULL) {
  res <- read_res(name)$units
  res <- sapply(res, nchar) > 0
  all(res, na.rm = TRUE)
}

#' @export
#' @rdname is_identical_to_ref
has_units_any <- function(name, part = NULL) {
  res <- read_res(name)$units
  res <- sapply(res, nchar) > 0
  any(res, na.rm = TRUE)
}

#' @export
#' @rdname is_identical_to_ref
#' @param text The text to check
#' @param object The object to look for in the `eq__()` function
is_display_equation <- function(text, object) {
  reg_exp <- paste0("`r +eq__\\(", object, "\\)`")
  grepl(reg_exp, text) |> any()
}

#' @export
#' @rdname is_identical_to_ref
is_display_param_equation <- function(text, object) {
  reg_exp <- paste0("`r +eq__\\(", object, ", +use_coefs *= *TRUE[^`]*\\)`")
  grepl(reg_exp, text) |> any()
}

#' @export
#' @rdname is_identical_to_ref
is_inline_equation <- function(text, object) {
  reg_exp <- paste0("`r +eq_\\(", object, "\\)`")
  grepl(reg_exp, text) |> any()
}

#' @export
#' @rdname is_identical_to_ref
is_inline_param_equation <- function(text, object) {
  reg_exp <- paste0("`r +eq_\\(", object, ", +use_coefs *= *TRUE[^`]*\\)`")
  grepl(reg_exp, text) |> any()
}
