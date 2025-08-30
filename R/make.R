

#' Run action of a MAKEFILE from R
#'
#' @description
#' The five possible actions are "test", to launch the test on the repository,
#' "clean" to delete several generated files, "original" to configure the
#' repository with original files, "solution" to configure the repository with
#' solution files, and "prepare" to prepare the repository for the final version
#' ready for the assignment in, say Github classroom.
#'
#' @return The result from `make` is returned
#' @export
make_test <- function() {
  odir <- setwd(fs::path(rprojroot::find_package_root_file(), "tests"))
  on.exit(setwd(odir))
  system("make -s test")
}

#' @export
#' @rdname make_test
make_clean <- function() {
  odir <- setwd(fs::path(rprojroot::find_package_root_file(), "tests"))
  on.exit(setwd(odir))
  system("make -s clean")
}

#' @export
#' @rdname make_test
make_original <- function() {
  odir <- setwd(fs::path(rprojroot::find_package_root_file(), "tests"))
  on.exit(setwd(odir))
  system("make -s original")
}

#' @export
#' @rdname make_test
make_solution <- function() {
  odir <- setwd(fs::path(rprojroot::find_package_root_file(), "tests"))
  on.exit(setwd(odir))
  system("make -s solution")
}

#' @export
#' @rdname make_test
make_prepare <- function() {
  odir <- setwd(fs::path(rprojroot::find_package_root_file(), "tests"))
  on.exit(setwd(odir))
  system("make -s prepare")
}
