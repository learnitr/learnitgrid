#' Summarize the git stats for one or more git repositories
#'
#' @description Use data from a git_stats.csv file to get a history of commits
#'   done in a repository for the learnitgrid Shiny app.
#'
#' @param git_stats_file The path the the git_stats.csv file
#' @param exclude_authors The list of authors to exclude from the stats
#' @param type The type of files to consider ("all", "R", "Rmd", or "Qmd")
#' @param tz The time zone to use for times
#'
#' @return A data frame with git stats data to be used by the learnitgrid Shiny
#'   app.
#' @export
get_git_stats <- function(git_stats_file,
exclude_authors = "github-classroom[bot]", type = "all", tz = "UTC") {

  if (!file_exists(git_stats_file))
    stop("The file ", git_stats_file, " does not exist")
  stat <- suppressMessages(read.csv(git_stats_file))

  # TODO: allow for more types too (e.g., doc, docx, pdf, ppt, pptx, ...)
  vec <- match.arg(type, choices = c("all", "R", "Rmd", "Qmd"),
    several.ok = TRUE)

  if (length(exclude_authors))
    stat <- stat[!stat$author %in% exclude_authors, ]

  if (!any(vec == "all"))
    stat <- stat[stat$extension %in% vec, ]
  #message("items in git_stats.csv: ", nrow(stat))

  stat <- collapse::roworder(stat, 'github_repository', 'author', 'author_date')
  g <- collapse::GRP(stat, ~ github_repository + author)
  stat$change_cum <- collapse::fcumsum(stat$change, g)
  g1 <- collapse::GRP(stat, ~ github_repository + author + extension)
  stat$change_cum1 <- collapse::fcumsum(stat$change, g1)
  #stat$github_repository2 <- tolower(stat$github_repository)
  stat$author_date <- with_tz(
    ymd_hms(stat$author_date,tz = "UTC"), tz)
  stat$author_date <- ymd_hms(as.character(stat$author_date))

  stat
}
