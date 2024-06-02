#' Run the learnitgrid Shiny application
#'
#' @param data_dir The directory containing the 'learnitgrid' data. If missing,
#'   the "learnitgrid.data.dir" option is used. If `""`, the small example
#'   dataset in this package is used.
#'
#' @return Nothing is returned, the function is used for its side-effect of
#'   running the learnitgrid Shiny application.
#' @seealso [install_grid_example()]
#' @export
#'
#' @examples
#' \dontrun{
#' library(learnitgrid)
#' # Run a basic example in the learnitgrid package
#' run_grid("")
#' # Please, note that the .Rmd reports that are analyzed in this example
#' # are NOT included to save space, hence, error messages in corresponding
#' # columns of the evaluation grids.
#'
#' # Install an extended example in tempdir() (inspect its content)
#' install_grid_example(browse = TRUE)
#' # ... and run the app with it
#' run_grid()
#' # The .Rmd reports are included in this example.
#' }
run_grid <- function(data_dir = getOption("learnitgrid.data.dir", NULL)) {
  if (missing(data_dir))
    data_dir <- getOption("learnitgrid.data.dir", NULL)
  if ( is.null(data_dir) || data_dir == "") # Use the small package example
    data_dir <- system.file('shiny', 'grid', 'www', package = 'learnitgrid')

  if (!length(data_dir) || data_dir == "" || !dir.exists(data_dir))
    stop("data_dir = must be the directory containing the learnitgrid data")

  # The directory with the data to use is set this way:
  options(learnitgrid.data.dir = data_dir)

  app = system.file('shiny', 'grid',  package = 'learnitgrid')
  shiny::runApp(app)
}
