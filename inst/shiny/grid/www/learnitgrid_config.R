# Learnitgrid configuration file
# Place here any constant or variable definitions that you need for your
# learnitgrid application

# Non-standard repos and assign files
repos_filename    <- "repositories.csv"
assign_filename   <- "assignments.csv"

# GitHub related constants
github_org        <- "BioDataScience-Course"
github_url        <- paste0("https://github.com/", github_org)
branch            <- "main"

# Various data relative to the courses
courses_aliases <- c(  # Alias for the courses in the dropdown box
  "A22M - Science des données I" = "A22M",
  "B22M - Sciences des données II" = "B22M",
  "C22M - Sciences des données III" = "C22M")
classroom_urls <- c(   # Links to the GitHub Classroom pages
  A = "https://classroom.github.com/classrooms/xxx/",
  B = "https://classroom.github.com/classrooms/xxx/",
  C = "https://classroom.github.com/classrooms/xxx/")

# stat_git() options
exclude_authors   <- c("sdd", "github-classroom[bot]")
git_stats_type    <- c("all", "R", "Rmd", "Qmd")
git_stats_tz      <- "Europe/Paris"
