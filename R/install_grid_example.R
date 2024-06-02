#' Install and extended example dataset to try the learnitgrid Shiny application
#'
#' @param dir The directory where to decompress the extended example
#' @param browse Should we browse the example directory after decompression?
#'
#' @return The path to the example learnitgrid data is returned invisibly.
#' @seealso [run_grid()]
#' @export
#'
#' @examples
#' # Install the extended examples in a temporary directory
#' \dontrun{
#' library(learnitgrid)
#' ex_dir <- install_grid_example(browse = TRUE)
#' }
install_grid_example <- function(dir = tempdir(), browse = FALSE) {
  if (is.null(dir) || !length(dir) || !is.character(dir) || !dir.exists(dir))
    stop("dir must be a single existing directory")

  # Decompress the example archive
  ex_archive <- system.file('example', 'learnitgrid-example.tar.xz',
    package = 'learnitgrid')
  ex_dir <- file.path(dir, 'learnitgrid-example', 'www')
  if (!dir.exists(ex_dir))
    untar(ex_archive, exdir = dir)
  if (!dir.exists(ex_dir))
    stop("example data was not decompressed correctly")

  # Put the path in the options
  options(learnitgrid.data.dir = as.character(ex_dir))

  if (isTRUE(browse)) {
    message("Browsing ", ex_dir, "...")
    browseURL(ex_dir)
  }

  invisible(ex_dir)
}
