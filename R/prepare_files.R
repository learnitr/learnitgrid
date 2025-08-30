#' Prepare files for original or solution version
#'
#' @description
#' Make sure all files are original or solution versions, and possibly also
#' remove last_saved versions. This is typically used to prepare the repository
#' for an assignment, or to switch from originals to solution to verify the
#' tests.
#'
#' @param type Either `"original"` or `"solution"`
#' @param remove_last_saved Should we also remove the last_saved version
#'   (`FALSE` by default)?
#' @param error If `TRUE`, an error is thrown in case there is no file to
#' prepare
#'
#' @return `NULL`
#' @export
prepare_files <- function(type = "original", remove_last_saved = FALSE,
  error = TRUE) {
  if (type != "original" && type != "solution")
    stop("type must be either original or solution")
  # Get a list of original/solution files
  files <- fs::dir_ls(rprojroot::find_package_root_file(),
    all = TRUE, recurse = TRUE, type = "file",
    regexp = paste0("_", type, "\\.[a-zA-Z0-9]+$"))
  if (!length(files)) {# There MUST be at least one orig|solut file
    if (isTRUE(error)) {
      stop("No ", type, " files found")
    } else {
      return(NULL)
    }
  }

  prepare_one <- function(file) {
    dir <- dirname(file)
    # We need to remove the leading dot and "_original|_solution" in filename
    orig_filename <- basename(file)
    reg_exp <- paste0("^\\.(.+)(_", type, ")(\\.[a-zA-Z0-9]+)$")
    work_filename <- sub(reg_exp, "\\1\\3", orig_filename)
    # If the filename is not changed, there is a problem with the name !
    if (work_filename == orig_filename)
      stop("Cannot match ", type, " and working version for ", orig_filename)
    work_file <- fs::path(dir, work_filename)
    # If extension is .Rscript, we need to change it to .R
    if (endsWith(file, ".Rscript"))
      work_file <- sub("\\.Rscript$", ".R", work_file)
    # Copy original|solution into working version
    fs::file_copy(file, work_file, overwrite = TRUE)
    # Do we also need to eliminate last_saved version, if present?
    if (isTRUE(remove_last_saved)) {
      saved_file <- sub(paste0("_", type), "_last_saved", file)
      if (saved_file != file && fs::file_exists(saved_file))
        fs::file_delete(saved_file)
    }
    file
  }
  unlist(purrr::map(files, prepare_one))
}

