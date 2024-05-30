# Summarize and check evaluation grids (corrections) for learnitr projects
# Copyright (c) 2021-2022, Philippe Grosjean & Guyliann Engels
#
# Warning: correction for grid_one when grid is a data.table! and NA everywhere -> NA

# At the point, all the evaluations must be filled. We check there is no missing
# scores and we calculate the final score for each student. The results are
# printed on screen and saved in a .log file in the same directory.

#res <- stat_git(git_stats, type = c("Rmd", "R"))



#' Summarize and check evaluation grids for the learnitgrid Shiny app
#'
#' @description
#' This function checks all evaluation grids in a correction set and computes
#' the required statistics for the summary page of the learnitgrid shiny app.
#'
#' @param dir The path to the evaluation grids
#' @param save.log Should a log file be saved (yes by default)?
#' @param save.rds Should an RDS file with the data be saved (yes by default)?
#'
#' @return A data frame with the results of the evaluation grids check for the
#'   learnitgrid Shiny app.
#' @export
check_grids <- function(dir, save.log = TRUE, save.rds = TRUE) {
  stopifnot(is.character(dir), length(dir) == 1, dir_exists(dir))

  # Get the assignment name
  assignment <- substring(basename(dir), 1, nchar(basename(dir)) - 11)

  # Get a list of all .csv files
  grids <- dir_ls(dir, glob = "*.csv")
  if (!length(grids))
    stop("'No correction grids found in the directory ", dir)

  # Initialize the results
  res <- data.frame(
    assignment = character(0),
    team       = character(0),
    student    = character(0),
    score_20   = numeric(0),
    score      = numeric(0),
    max        = numeric(0),
    missing    = numeric(0),
    wrong      = numeric(0),
    file       = character(0)
  )

  for (i in 1:length(grids)) {
    # Get the team name (= user login for individual projects)
    team <- substring(basename(grids[i]), nchar(assignment) + 2,
      nchar(basename(grids[i])) - 4)

    # Get and process the grid
    grid <- suppressMessages(read.csv(grids[i]))
    # If the column student is missing, create one filled with ""
    if (is.null(suppressWarnings(grid$student)) ||
        all(grid$student == "", na.rm = TRUE)) {
      suppressWarnings(grid$student <- NULL)
      grid$student <- ""
      students <- team # single student in the team with login = team name
    } else {
      # Replace missing values by ""
      grid$student[is.na(grid$student)] <- ""
      # Get a list of students in the team
      students <- unique(grid$student)
      students <- students[students != ""]
    }
    # Create a results table for this team
    res2 <- data.frame(
      assignment = assignment,
      team       = team,
      student    = students,
      score_20   = as.numeric(NA),
      score      = as.numeric(NA),
      max        = as.numeric(NA),
      missing    = 0,
      wrong      = 0,
      file       = basename(grids[i])
    )

    # Calculate scores for each student
    for (j in 1:length(students)) {
      student <- students[j]
      sel_student <- grid$student %in% c("", student)
      grid_one <- grid[sel_student, ]
      is_missing <- sum(is.na(grid_one$score))
      res2$missing[j] <- is_missing
      if (!is.numeric(grid_one$score)) {
        grid_one$score <- suppressWarnings(as.numeric(grid_one$score))
        res2$wrong[j] <- sum(is.na(grid_one$score)) - is_missing
      }
      # In case of missing values, we keep NA instead (student that does not
      # have NA score for all items!)
      #res2$score[j] <- max(0, sum(grid_one$score, na.rm = TRUE))
      res2$score[j] <- sum(grid_one$score, na.rm = FALSE)
      res2$max[j] <- sum(grid_one$max)
      res2$score_20[j] <- round(res2$score[j] / res2$max[j] * 20, 1)
    }

    res <- rbind(res, res2)
  }

  attr(res, "date") <- as.character(Sys.time())

  # Do we save the log on disk?
  if (isTRUE(save.log)) {
    lines <- paste0(
      format(res$team, width = 20), " ",
      format(res$student, width = 20), " ",
      format(as.character(res$score_20), width = 4), "/20    (",
      format(paste0(res$score, "/", res$max), width = 8), " - ",
      res$missing, " missing, ", res$wrong,
      " incorrect scores)   ", res$file
    )
    writeLines(lines, con = path(dir, "summary.log"))
  }

  if (isTRUE(save.rds)) {
    saveRDS(res, path(dir, "summary.rds"))
  }

  # Check a couple of cases:
  cat(length(grids), " correction grids checked.\n")
  cat(nrow(res), " students are in the projects.\n")
  cat(sum(!is.na(res$score_20)), " students are scored.\n")
  if (length(unique(res$max)) != 1)
    message("Error: not all correction grids have same max score.")
  if (sum(res$missing) > 0)
    message("Warning: there is a total of ", sum(res$missing),
      " missing scores.")
  if (sum(res$wrong) > 0)
    message("Warning: there is a total of ", sum(res$wrong),
      " incorrect scores.")
  cat(sum(res$score_20 < 10, na.rm = TRUE), " students did not succeed.\n")
  cat("\nThe scores /20 distribute like this (min, q1, median, q3, max):\n")
  print(fivenum(res$score_20))

  invisible(res)
}
