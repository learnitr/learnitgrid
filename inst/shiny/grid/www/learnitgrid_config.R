# Learnitgrid configuration file
# Place here any constant or variable definitions that you need for your
# learnitgrid application

# Non-standard repos and assign files
repos_filename    <- "repositories.csv"
assign_filename   <- "assignments.csv"

# Global context (your app, your online course and your student's mail domain)
app_url           <- "https://github.com/learnitr/learnitgrid"
web_url           <- "https://learnitr.github.io/learnitrdoc/"
mail_domain       <- "university.edu"

# GitHub related constants
github_org        <- "BioDataScience-Course"
github_url        <- paste0("https://github.com/", github_org)
branch            <- "main"
docs_on_github    <- FALSE # Use GitHub or local version of the documents?

# Various data relative to the courses
courses_aliases <- c(  # Alias for the courses in the dropdown box
  "A22M - Science des données I" = "A22M",
  "B22M - Sciences des données II" = "B22M",
  "C22M - Sciences des données III" = "C22M")
classroom_urls <- c(   # Links to the GitHub Classroom pages
  A = "https://classroom.github.com/classrooms/xxx/",
  B = "https://classroom.github.com/classrooms/xxx/",
  C = "https://classroom.github.com/classrooms/xxx/")

# UI options
max_lines         <- 30L
default_highlight <- FALSE # Nice but slow -> FALSE by default!

# stat_git() options
exclude_authors   <- c("sdd", "github-classroom[bot]")
git_stats_type    <- c("all", "R", "Rmd", "Qmd")
git_stats_tz      <- "Europe/Paris"
