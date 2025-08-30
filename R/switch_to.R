#' Switch to a given version of the document or save such a version
#'
#' @description When there are several versions of a document (R script, R
#'   Markdown or Quarto document), switch the main file to one of the saved
#'   versions: original version, solution with the correction, or last saved
#'   version. The `save_as_original()` and `save_as_solution()` functions are
#'   used to create respectively the original and solution versions of the
#'   document. These files have same name as the initial file but ending with
#'   `_original`, `_solution` or `_last_saved`., and starting with a dot `.`.
#'   They are thus "hidden" files. For R scripts, the extension is also changed
#'   into `.Rscript` for the original and solution versions.
#'
#' @param file The file to switch to a version. If not provided, the currently
#'   edited file in RStudio is used by default
#' @param error In case the version does not exists, do we issue an error or not
#'   (no by default)
#'
#' @return `NULL` is returned invisibly. The functions are used for their
#'   side-effect of switching document versions.
#' @export
switch_to_original <- function(file = NULL, error = TRUE) {
  file <- .check_file(file)
  orig_file <- attr(file, "original")
  if (!fs::file_exists(orig_file)) {
    if (isTRUE(error)) {
      stop("There is no original file ", basename(orig_file), " available")
    } else {# Silently return NULL
      return(invisible(NULL))
    }
  }
  # Make a backup of current version in last_saved first
  fs::file_copy(file, attr(file, "last_saved"), overwrite = TRUE)
  fs::file_copy(orig_file, file, overwrite = TRUE)
  invisible(orig_file)
}

#' @export
#' @rdname switch_to_original
switch_to_solution <- function(file = NULL, error = TRUE) {
  file <- .check_file(file)
  solut_file <- attr(file, "solution")
  if (!fs::file_exists(solut_file)) {
    if (isTRUE(error)) {
      stop("There is no solution file ", basename(solut_file), " available")
    } else {# Silently return NULL
      return(invisible(NULL))
    }
  }
  # Make a backup of current version in last_saved first
  fs::file_copy(file, attr(file, "last_saved"), overwrite = TRUE)
  fs::file_copy(solut_file, file, overwrite = TRUE)
  invisible(solut_file)
}

#' @export
#' @rdname switch_to_original
switch_to_last_saved <- function(file = NULL, error = TRUE) {
  file <- .check_file(file)
  saved_file <- attr(file, "last_saved")
  if (!fs::file_exists(saved_file)) {
    if (isTRUE(error)) {
      stop("There is no last saved file ", basename(saved_file), " available")
    } else {# Silently return NULL
      return(invisible(NULL))
    }
  }
  # We will interchange current and last_saved, be need to temporary rename
  # last_saved into .tmp
  tmp_file <- paste0(saved_file, ".tmp")
  fs::file_copy(saved_file, tmp_file, overwrite = TRUE)
  fs::file_copy(file, saved_file, overwrite = TRUE)
  fs::file_copy(tmp_file, file, overwrite = TRUE)
  fs::file_delete(tmp_file)
  invisible(saved_file)
}

#' @export
#' @rdname switch_to_original
save_as_original <- function(file = NULL) {
  file <- .check_file(file)
  fs::file_copy(file, attr(file, "original"), overwrite = TRUE)
}

#' @export
#' @rdname switch_to_original
save_as_solution <- function(file = NULL) {
  file <- .check_file(file)
  fs::file_copy(file, attr(file, "solution"), overwrite = TRUE)
}

# Get the currently edited file if file == NULL and check it exists and it does
# not starts with a dot (.)
# Also compute the filepath for original, solution and last_saved versions
.check_file <- function(file = NULL) {
  if (is.null(file)) {
    if (!rstudioapi::isAvailable())
      stop("No file provided and not in Rstudio")
    file <- try(rstudioapi::documentPath(), silent = TRUE)
    # If I got an error, either there is no document currently edited, or the
    # document has not been saved yet
    if (inherits(file, "try-error"))
      stop("No file provided and no file currently edited, or the edited file is not saved yet.")
    # Save the document to make sure we have latest version
    rstudioapi::documentSave()
  }
  stopifnot(is.character(file), length(file) == 1, nchar(file) > 0)
  if (!fs::file_exists(file))
    stop("File not found")
  filename <- basename(file)
  dirname <- dirname(file)
  if (substring(filename, 1L, 1L) == ".")
    stop("You indicate a file starting with a dot (.), maining it is probably not a working version")
  # Compute names for original, solution, and last_saved versions of this file
  ext <- fs::path_ext(filename)
  # In case of R scripts, we use .Rscript instead of .R for original and solution
  if (ext == "R") ext2 <- "Rscript" else ext2 <- ext
  basename <- paste0(".", fs::path_ext_remove(filename))
  orig_filename <- paste0(basename, "_original.", ext2)
  attr(file, "original") <- fs::path(dirname, orig_filename)
  solut_filename <- orig_filename <- paste0(basename, "_solution.", ext2)
  attr(file, "solution") <- fs::path(dirname, solut_filename)
  saved_filename <- orig_filename <- paste0(basename, "_last_saved.", ext2)
  attr(file, "last_saved") <- fs::path(dirname, saved_filename)

  file
}
