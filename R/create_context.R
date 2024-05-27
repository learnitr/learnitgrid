#' Create a context object for a correction set
#'
#' @description
#' Create a list that contains context for a given correction set (must be
#' recalculated when a different correction set/project is selected).
#'
#' @param correction The correction set
#' @param base_corr_dir The base directory for correction sets
#' @param base_templ_dir The base directory for correction templates
#' @param base_repos_dir The vase directory for GitHub repositories to correct
#' @param repositories The repositories
#' @param assignments The assignments
#' @param github_url The GitHub URL (base part)
#' @param branch The GitHub branch concerned by this correction
#'
#' @return A list with the context information for the learnitgrid Shiny app.
#' @export
create_context <- function(correction, base_corr_dir, base_templ_dir,
base_repos_dir, repositories, assignments, github_url, branch) {
  course_dirs <- dir_ls(base_corr_dir, type = "directory")
  if (!length(course_dirs))
    stop("There is still no evalaution grids for this assignment. ",
      "Use the script '04create_assessments4.R' to create them...")
  courses <- basename(course_dirs)

  corr_dirs <- dir_ls(course_dirs, type = "directory")
  if (!length(corr_dirs))
    stop("There is still no evalaution grids for this assignment. ",
      "Use the script '04create_assessments4.R' to create them...")
  corrections <- basename(corr_dirs)

  if (is.null(correction) || !length(correction) || correction == "")
    stop("Correction set not provided, or operation cancelled.")
  if (!correction %in% corrections)
    stop("Correction set '", correction, "' not found. Check it...")

  # Get corr_dir for this correction + get course, assignment and corr_date
  corr_dir <- corr_dirs[corrections == correction]
  course <- basename(dirname(corr_dir))
  corr_parts <- strsplit(correction, "_", fixed = TRUE)[[1]]
  if (length(corr_parts) < 4)
    stop("Problem with the correction set '", correction,
      "'  : it should be something like A00Ia_21M_title_2022-01-01")
  corr_date  <- corr_parts[length(corr_parts)]
  assignment <- paste(corr_parts[-length(corr_parts)], collapse = "_")
  rm(corr_parts)

  # Get dirs, files and more for this correction set
  templ_file <- file_path_check(base_templ_dir, course,
    paste0(assignment, ".csv"))
  repos_dir <- dir_path_check(base_repos_dir, course, assignment)
  corr_files <- dir_ls(corr_dir, glob = '*.csv')
  if (!length(corr_files))
    stop("Correction files not found in '", corr_dir, "' !")
  n <- length(corr_files)
  repos_names <- sub("\\.csv$", "", basename(corr_files))
  # PhG: adapted for urchins in Q2
  #repos_names2 <- paste0(substring(repo, 1L, 3L), substring(repos_names, 4L))
  repos_names2 <- repos_names
  # PhG: in case this is a pseudonymised version, we got either the original
  # GitHub repos, or the pseudonymized one from repositories_xx.csv
  if (is.null(repositories$pseudo)) {
    message("Use regular repositories names")
    repos_names3 <- repos_names2
  } else {
    message("Pseudonymised data with regular repostories")
    rnames <- repositories$name
    names(rnames) <- tolower(repositories$pseudo)
    repos_names3 <- rnames[tolower(repos_names2)]
    names(repos_names3) <- NULL
  }

  repos_dirs <- path(repos_dir, repos_names)
  repos_dirs2 <- path(repos_dir, repos_names2)
  user_logins <- substring(repos_names, nchar(assignment) + 2)

  # General assignment infos from assign_file
  assign_infos <- as.data.frame(assignments)[tolower(assignments$assignment) ==
      tolower(assignment), ]
  # Also filter on app, if provided
  #if (!is.null(app))
  #  assign_infos <- assign_infos[tolower(assign_infos$app) == tolower(app), ]
  if (!NROW(assign_infos)) {
    stop("The selected assignment (", assignment, ") is not found in ",
      "the table.")
  }

  if (NROW(assign_infos) > 1) {
    warning("The selected assignment (", assignment,
      ") is found in several variants in the table. Use of the first one.")
    assign_infos <- assign_infos[1, ]
  }

  # Read information from the template file
  templ_corrs <- suppressMessages(read(templ_file))
  if (NROW(templ_corrs) < 1)
    stop("No correction items found in '", templ_file,
      "', or error reading the file.")

  # Eliminate user-specific items (starting with !{login} in the template)
  templ_corrs <-
    templ_corrs[substring(templ_corrs$criterion, 1, 8) != "!{login}", ]

  # Path of git_stat.csv
  git_stats <- file_path_check(repos_dir, "git_stats.csv")

  # The context object (list) contains info required to populate tables
  context <- list(
    assignment = assignment,     # The corresponding assignment (project)
    templ_corrs = templ_corrs,   # The content of the correction grid template
    corr_files = corr_files,     # The correction grids files (CSV)
    assign_infos = assign_infos, # Informations about the assignment
    github_url = github_url,     # The base URL for GitHub links
    branch = branch,             # The GitHub branch
    git_stats_file = git_stats,  # The path to git_stats.csv file
    user_logins = user_logins,   # The users logins (teams for group projects)
    repos_names = repos_names,   # The names of repositories
    repos_names2 = repos_names2, # The names of repos if different from apps
    repos_names3 = repos_names3, # The name of original repos (pseudonymisation)
    repos_dirs = repos_dirs,     # The paths to the repositories
    repos_dirs2 = repos_dirs2    # The paths to repos if different from apps
  )
  context
}
