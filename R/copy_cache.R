
#' Copy data cache from a reference folder into an RStudio project
#'
#' @description
#' When GitHub repositories for a Classroom assessment are cloned, the data
#' cache is not present. For large datasets, it may take a long time to get
#' the data from an URL for each repository. By using a single version of the
#' data cache, on can populated the corresponding folder in each repository.
#' The `CC()` function can be used to remove the data from the cache.
#'
#' @return The list of copied files is returned invisibly.
#' @export
copy_cache <- function() {
  # In the correction folder, we have something like:
  # .../repos/<course>/<repo>/<repo>-<login>/tests
  # The data cache is in:
  # .../repos/<course>/<repo>_data/data_cache
  # <repo> is deduced from the name of the .Rproj file
  repo <- fs::dir_ls(".", type = "file", glob = "*.Rproj") |>
    fs::path_ext_remove()
  if (!length(repo)) return() else repo <- repo[1]
  # data_dir has a name like <repo>_data
  data_dir <- paste0(repo, "_data")
  if (fs::dir_exists(fs::path("..", data_dir))) {
    data_dir <- fs::path("..", data_dir)
  } else if (fs::dir_exists(fs::path("../..", data_dir))) {
    data_dir <- fs::path("../..", data_dir)
  } else {# data_dir not found
    return()
  }
  message("Getting data cache from: ", data_dir)
  # The folder where the data cache should be placed
  fs::dir_create("data/data_cache/")
  data_files <- fs::dir_ls(data_dir, all = TRUE, type = "file")
  for (data_file in data_files)
    fs::file_copy(data_file, "data/data_cache/", overwrite = TRUE)
  invisible(data_files)
}
cache_data <- copy_cache()

#' @rdname copy_cache
#' @export
#' @param only_if_copied If `TRUE`, the cache is only deleted if it was copied
#' @param cache_data The list of copied files, returned by `copy_cache()`
CC <- clean_cache <- function(only_if_copied = FALSE, cache_data = cache_data) {
  if (isTRUE(only_if_copied) && is.null(cache_data)) # Nothing copied, so do not delete
    return()
  if (fs::dir_exists("data/data_cache/"))
    fs::dir_delete("data/data_cache/")
}

