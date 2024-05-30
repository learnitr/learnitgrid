# Packages required -------------------------------------------------------

library(shiny)
library(shinydashboard)
library(learnitgrid)


# Global constants --------------------------------------------------------

# Absolute path to the data directory, use environment variable on RSConnect
# and default value (unset = ) locally
data_dir          <- Sys.getenv("LEARNITGRID_DATA_DIR",
                       unset = getOption("learnintgrid.data.dir",
                         fs::path(getwd(), "www")))
root_dir        <- dir_path_check(data_dir)

# Possibly input these values from a learnitgrid_config.R either in root_dir
# or (if not present) in www
config_file <- fs::path(root_dir, "learnitgrid_config.R")
if (!fs::file_exists(config_file))
  config_file <- fs::path("www", "learnitgrid_config.R")
if (fs::file_exists(config_file)) {
  message("Sourcing config file ", config_file, "...")
  source(config_file, encoding = "UTF-8") #, echo = TRUE)
} else {
  message("No config file ", config_file, " found!")
}

# Subdirectories
if (!exists("base_corr_dir"))
  base_corr_dir   <- dir_path_check(root_dir,
                       getOption("learnitigrid.corrections", "corrections"))
if (!exists("base_templ_dir"))
  base_templ_dir  <- dir_path_check(root_dir,
                       getOption("learnitigrid.templates", "templates"))
if (!exists("base_repos_dir"))
  base_repos_dir  <- dir_path_check(root_dir,
                       getOption("learnitigrid.repos", "repos"))
# Shiny apps can only share files relatives to its www subdir. So, we (re)create
# simlinks to these three folders (but do not delete if it is an existing file)
link_to_www(base_corr_dir, "corr")
link_to_www(base_templ_dir, "temp")
link_to_www(base_repos_dir, "repo")

# TODO: this could be located possibly in a database too (then create a
# snapshot in a file in learnitgrid_config.R?)
if (!exists("repos_filename"))
  repos_filename  <- Sys.getenv("LEARNITGRID_REPOS",
                       unset = "repositories.csv")
if (!exists("assign_filename"))
  assign_filename <- Sys.getenv("LEARNITGRID_ASSIGN",
                       unset = "assignments.csv")

if (!exists("github_org"))
  github_org      <- Sys.getenv("LEARNITGRID_ORG",
                       unset = getOption("learnitgrid.org",
                         "learnitgrid-test"))
if (!exists("github_url"))
  github_url      <- paste0("https://github.com/", github_org)
# TODO: the branch should be deduced from the set name (a branch or a date)
# ... or use the default branch (note: master is also OK here)
if (!exists("branch"))
  branch          <- getOption("learnitgrid.branch", "main")

# Some more info about courses (no data by default)
if (!exists("courses_aliases"))
  courses_aliases <- NULL   # The aliases in tnhe dropdown box for your courses
if (!exists("classroom_urls")) {
  classroom_urls  <- rep("https://classroom.github.com/classrooms/???/", 26L)
  names(classroom_urls) <- LETTERS # Default URLS for classrooms => unknown
}

# TODO: make these two options in the UI
# Maximum number of lines to place in content
if (!exists("max_lines"))
  max_lines       <- getOption("learnitgrid.maxlines", 20L)
# This one is nice, but slow => better to leave it FALSE
if (!exists("default_highlight"))
  default_highlight <- getOption("learnitgrid.highlight", FALSE)

# Default options for get_git_stats()
if (!exists("exclude_authors"))
  exclude_authors <- "github-classroom[bot]"
if (!exists("git_stats_type"))
  git_stats_type  <- "all"
if (!exists("git_stats_tz"))
  git_stats_tz    <- "UTC" # Your own tz, e.g., "Europe/Paris"

# Read csv files without issuing messages
read <- function(file) {
  #suppressMessages(data.io::read(file))
  read.csv(file)
}

# Global objects ----------------------------------------------------------

repos_file        <- file_path_check(root_dir, repos_filename)
repositories      <- suppressMessages(read.csv(repos_file))
assign_file       <- file_path_check(root_dir, assign_filename)
assignments       <- suppressMessages(read.csv(assign_file))
order             <- NULL     # The order for the table by criterion

course_dirs <- fs::dir_ls(base_corr_dir, type = "directory")
if (!length(course_dirs))
  stop("Il n'y a pas encore de grilles de correction disponibles. ",
    "Créez-en avant de relancer cette application...")
courses <- basename(course_dirs)
courses_list <- courses
names(courses_list) <- courses
courses_list <- c(courses_aliases,
  courses_list[!courses_list %in% courses_aliases])
courses_list <- courses_list[courses_list %in% courses]

# TODO: get login of user if app is run in RStudio Connect
default_evaluator <-  try(gh::gh_whoami()$login, silent = TRUE)
if (inherits(default_evaluator, 'try-error') || is.null(default_evaluator)) {
  default_evaluator <- ""
} else {
  default_evaluator <- as.character(default_evaluator)[1]
}

# Sort the table according to content similarities
# Note: depends on a global variable `order` initialized to `NULL`!
# For this reason, it cannot be relocated in learnitgrid_functions.R
sort_table <- function(x, is_content = attr(x, "is_content")) {
  # Calculate a dissimilarity matrix with string distances ('osa' method)
  # TODO: try other methods as well
  if (isTRUE(is_content)) {
    n <- nrow(x)
    dissim_mat <- matrix(0, nrow = n, ncol = n)
    content <- x$content
    stringsim <- stringdist::stringsim
    for (i in 1:n)
      dissim_mat[i, ] <- 1 - stringsim(content, content[i], method = "osa")
    # Create a dist object by keeping only the lower triangle
    dissim <- as.dist(dissim_mat)
    # Cluster this dist object, in order to sort items by dissimilarities
    cl <- flashClust::hclust(dissim, method = "complete")
    #plot(cl) # TODO: we could display this in a plot above the grid
    # We are only interested by order in cl
    order <- cl$order
    # Reorder x accordingly
    x <- x[order, ]
  } else {
    order <- 1:nrow(x) # Initial order simply
  }
  order <<- order # TODO: do this differently!
  attr(x, "order") <- order
  x
}

# The Shiny app UI --------------------------------------------------------

header <- dashboardHeader(
  title =  a("Learnitgrid",
    href = "https://github.com/SciViews/learnitgrid"),# style = "color:black;"),
  titleWidth = 350,
  tags$li(a(
    onclick = "onclick=window.open('https://wp.sciviews.org/')",
    href = NULL, icon("book"), title = "Cours", style = "cursor:pointer;"),
    class = "dropdown"),
  tags$li(a(
    onclick = "onclick=window.open('https://github.com/BioDataScience-Course')",
    href = NULL, icon("github"), title = "GitHub", style = "cursor:pointer;"),
    class = "dropdown")
)

sidebar <- dashboardSidebar(width = 350,
  selectInput("course", label = "Cours :", choices = names(courses_list),
    width = 325),
  selectInput("correction", label = "Set de correction :", choices = NULL,
    width = 325),
  textInput("evaluator", "Evaluateur (login GitHub) :",
    value = default_evaluator, width = 325,
    placeholder = "Votre login (Github) comme évaluateur"),
  textOutput("evaluator_check"),
  hr(),
  sidebarMenu(id = "menu_item",
    menuItem("Résumé", tabName = "summary",
      icon = icon("calculator")),
    menuItem("Correction par grille", tabName = "correction_grid",
      icon = icon("tasks")),
    menuItem("Correction par critère", tabName = "correction_criterion",
      icon = icon("check-square"))
  )#,
  #hr(),
  #downloadButton("downloadData1", "Generate report")
)

body <- dashboardBody(
  #shinybusy::add_busy_spinner(spin = "fading-circle"),
  tags$head(
    tags$base(target = "_blank"),
    tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(30%);
             }
             "))
    ),
  tabItems(
    tabItem(tabName = "summary",
      tags$h2(icon("calculator"), " Résumé"),
      h5(textOutput("last_summary")),
      fluidRow(
        infoBoxOutput("correction_set1"),
        infoBoxOutput("template_link1"),
        infoBoxOutput("evaluator_name1")
      ),
      fluidRow(
        valueBoxOutput("n_students", width = 3),
        valueBoxOutput("n_grids", width = 3),
        valueBoxOutput("n_success", width = 3),
        valueBoxOutput("mean", width = 3),
      ),
      fluidRow(
        downloadButton("downloadcsv", "CSV", icon = NULL,
          style = "margin-left:15px;" ),
        downloadButton("downloadexcel", "Excel", icon = NULL),
        actionButton("refresh", "Rafraichir", class = "btn-info")
      ),
      br(),
      fluidRow(
        box(title = "Notes de la classe", status = "info",
          shinycssloaders::withSpinner(plotOutput("score_plot"))),
        box(title = "Détails par étudiant", status = "info",
          shinycssloaders::withSpinner(DT::dataTableOutput("summary_tab")))
      )
    ),

    tabItem(tabName = "correction_grid",
      tags$h2(icon("tasks"), " Correction par grille"),
      h5(textOutput("to_corresct_per_grid")),
      fluidRow(
        infoBoxOutput("correction_set2"),
        infoBoxOutput("template_link2"),
        infoBoxOutput("evaluator_name2")
      ),
      fluidRow(
        box(title = "Contribution au projet", status = "info", width = 12,
          collapsible = TRUE, collapsed = TRUE,
          selectInput("contri_ext", label = "Type de fichiers",
            choices = c("Rmd", "R"), multiple = TRUE, selected = c("Rmd", "R")),
          radioButtons("contri_split", label = "",
            choices = c("unifier", "séparer"),
            selected = "unifier", inline = TRUE),
          #plotly::plotlyOutput("contri_plot"),
          plotOutput("contri_plot"),
        )
      ),
      fluidRow(
        box(title = "Grille critériée", status = "info", width = 12,
          selectInput("grid", "Grille critériée :", label = NULL,
            multiple = FALSE, selectize = TRUE, size = NULL, width = "50%",
            selected = NULL),
          #verbatimTextOutput("last_edited_grid"),
          #br(),
          shinycssloaders::withSpinner(
            DT::dataTableOutput('correction_table_grid', width = "100%")
          )
        )
      )
    ),

    tabItem(tabName = "correction_criterion",
      tags$h2(icon("check-square"), " Correction par critère"),
      h5(textOutput("to_correct_per_criterion")),
      fluidRow(
        infoBoxOutput("correction_set3"),
        infoBoxOutput("template_link3"),
        infoBoxOutput("evaluator_name3")
      ),
      fluidRow(
        box(title = "Entrées pour un critère", status = "info", width = 12,
          selectInput("item", "Critère :", label = NULL,
            multiple = FALSE, selectize = TRUE, size = NULL, width = "50%",
            selected = NULL),
          #verbatimTextOutput("last_edited"),
          #br(),
          shinycssloaders::withSpinner(
            DT::dataTableOutput('correction_table_criterion', width = "100%")
          )
        )
      )
    )
  )
)

shinyUI <-  dashboardPage(header, sidebar, body, skin = "black")


# The Shiny app server ----------------------------------------------------

shinyServer <- function(input, output, session) {
  # TODO: some temporary code for further enhancements
  # A modal dialog box prompting for evaluator login and correction set
  #modal_startup <- modalDialog(
  #  "Veuillez vérifier votre identité et sélectionner un set de correction",
  #  title = "Correction Science des Données Biologiques",
  #  footer = tagList(
  #    actionButton("cancel", "Annuler"),
  #    actionButton("ok", "Continuer", class = "btn btn-primary btn-lg")
  #  )
  #)

    # https://stackoverflow.com/questions/66262809/how-to-trigger-edit-on-single-click-in-r-shiny-dt-datatable
    # https://stackoverflow.com/questions/54907273/use-tab-to-edit-next-cell-on-dt-table
    js <- c(
      "table.on('click', 'td', function() {",
      "$(this).dblclick();",
      "});",
      "table.on('key', function(e, datatable, key, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  if(key == 13){",
      "    if(targetName == 'body'){",
      "      $(cell.node()).trigger('dblclick.dt');",
      "    }else if(targetName == 'input'){",
      "      $(originalEvent.target).trigger('blur');",
      "    }",
      "  }",
      "})",
      "table.on('keydown', function(e){",
      "  if(e.target.localName == 'input' && [9,13,37,38,39,40].indexOf(e.keyCode) > -1){",
      "    $(e.target).trigger('blur');",
      "  }",
      "});",
      "table.on('key-focus', function(e, datatable, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  var type = originalEvent.type;",
      "  if(type == 'keydown' && targetName == 'input'){",
      "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
      "      $(cell.node()).trigger('dblclick.dt');",
      "    }",
      "  }",
      "});")

  #showModal(modal_startup)

  #observeEvent(input$ok, {
  #  showNotification("Récupération+
  #  3, des données...")
  #  removeModal()
  #})
  #observeEvent(input$cancel, {
  #  removeModal()
  #  stopApp(1)
  #})

  grid_dir <- eventReactive(input$course, {
    # Select the course e.g. A22M, B22M,....
    course <- courses_list[input$course]
    x <- grep(course, course_dirs, value = TRUE)
    # Create a vector with de directory path
    res <- fs::dir_ls(x, type = "directory")
    names(res) <- basename(res)
    attr(res, "ntot") <- length(res)
    #message(glue::glue('
    #========================
    ##1 corr_dir1() = The list of directories by course.
    #{attr(res, "ntot")} folder(s) are to correct.
    #The first folder is {names(res)[1]}}
    #========================'))
    res
  })

  observe({
    req(input$course)
    updateSelectizeInput(session, 'correction', choices = names(grid_dir()))
  })

  context_react <- reactive({
    req(input$correction)

    default_correction <- input$correction
    create_context(correction = default_correction,
      base_corr_dir = base_corr_dir, base_templ_dir = base_templ_dir,
      base_repos_dir = base_repos_dir, repositories = repositories,
      assignments = assignments, #repo = repo, app = app,
      github_url = github_url, branch = branch)
  })


  # tabItem : summary ----

  summary_react <- reactive({
    req(input$correction)
    input$refresh

    course <- courses_list[input$course]
    dir <- fs::path(base_corr_dir, course, input$correction, "summary.rds")

    if (fs::file_exists(dir)) {
      res <- suppressMessages(readRDS(dir))
    } else {
      check_grids(fs::path(base_corr_dir, course, input$correction))
      res <- suppressMessages(readRDS(dir))
    }
    #message(glue::glue('
    #========================
    ##1 summary_react .
    #{dim(res)}
    #========================')
    #  )
    res
  })

  output$last_summary <- renderText({
    #req(input$correction)
    x1 <- attr(summary_react(), "date")
    glue::glue('Dernière mise à jour : {x1}.')
  })

  correction_set <- reactive({
    req(input$correction)
    context <- context_react()
    ref <- classroom_urls[names(classroom_urls) ==
        substr(input$correction, 1, 1)]
    ref1 <- fs::path(ref, "assignments",context$assignment[1])
    assignment_set <- input$correction
    curr_assign <- sub("^(.+)_([^_]+)$", "\\1", assignment_set)
    curr_set <- sub("^(.+)_([^_]+)$", "\\2", assignment_set)
    infoBox(h4(curr_assign), paste("Set:", curr_set),
      color = "blue", icon = icon("folder-open"), href = ref1, fill = TRUE)
  })

  output$correction_set1 <- renderInfoBox({correction_set()})

  template_link <- reactive({
    req(input$course)
    context <- context_react()
    templ_url <- context$assign_infos$template[1]
    templ_name <- basename(templ_url)
    infoBox(h4("Template"), templ_name,
      color = "blue", icon = icon("file-alt"), href = templ_url, fill = TRUE)
  })

  output$template_link1 <- renderInfoBox({template_link()})

  evaluator <- eventReactive(input$evaluator, {
    x <- input$evaluator
    res <- try(gh::gh("/users/{username}", username = x), silent = TRUE)
    attr(x, "exist") <- !inherits(res, 'try-error')
    x
  })

  evaluator_name <- reactive({
    req(input$course)
    x <- evaluator()
    if (!isTRUE(attr(x, "exist"))) {
      col <- "orange"
    } else {
      col <- "green"
    }
    infoBox(h4("Evaluateur"), x,
      color = col, icon = icon("user-check"),
      href = paste0("https://github.com/", x), fill = TRUE)
  })

  output$evaluator_name1 <- renderInfoBox({
    evaluator_name()
  })

  output$n_students <- renderValueBox({
    x <- summary_react()
    x1 <- length(unique(x$student))
    valueBox(x1, "Etudiants", icon = icon("users"), color = "blue")})

  output$n_grids <- renderValueBox({
    x <- summary_react()
    ntot <- length(unique(x$team))
    x1 <- x[x$missing >= 1, ]
    completed <- ntot - length(unique(x1$team))
    # Propose a three-colors coding instead
    if (ntot == completed) {
      col <- "green"
    } else {
      col <- "red"
    }
    res <- glue::glue("{completed} ({ntot - completed})")
    valueBox(res, "Grilles terminées (à faire)", icon = icon("check"),
      color = col)
  })

  output$n_success <- renderValueBox({
    x <- summary_react()
    x1 <- sum(x$score_20 >= 10, na.rm = TRUE)
    x2 <- sum(is.na(x$score_20))
    x3 <- nrow(x) - x2
    x4 <- round((x1 / sum(!is.na(x$score_20))) * 100, 0)
    if (is.na(x4)) {
      col <- "red"
    } else{
      col <- cut(x4,
        breaks = c(0, 74, 89, 100),
        labels = c("red", "orange", "green"))
    }
    if (x2 != 0) {
      res <- glue::glue("{x1}/{x3} ({x2})")
      res1 <- "Succès (non évalué)"
    } else {
      res <- glue::glue("{x1}/{x3}")
      res1 <- "Succès"
    }
    valueBox(res, res1, icon = icon("user-graduate"), color = col)
  })

  output$mean <- renderValueBox({
    x <- summary_react()
    x1 <- round(mean(x$score_20, na.rm = TRUE),1)
    x2 <- round(median(x$score_20, na.rm = TRUE),1)
    res <- glue::glue("{x1} ({x2})")
    valueBox(res, "Moyenne (Médiane) [/20]", icon = icon("equals"),
      color = "blue")
  })

  download <- reactive({
    res <- summary_react()
    res$email <- paste0(res$student, "@umons.ac.be")
    res1 <- res[,c("student", "email", "score_20", "file")]
    colnames(res1)[colnames(res1) == "score_20"] <- res$assignment[1]
    res1
  })

  output$downloadcsv <- downloadHandler(
    filename = function() {
      context <- context_react()
      paste0(context$assignment[1],"-summary-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(download(), file, row.names = FALSE)
    }
  )

  output$downloadexcel <- downloadHandler(
    filename = function() {
      context <- context_react()
      paste0(context$assignment[1],"-summary-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      #data.io::write$xlsx(download(), file)
      writexl::write_xlsx(download(), file)
    }
  )

  observeEvent(input$refresh, {
    course <- courses_list[input$course]
    check_grids(fs::path(base_corr_dir, course, input$correction))
  })

  output$score_plot <- renderPlot({
    req(input$correction)
    x <- summary_react()
    #x1 <- sum(!(x$missing > 1))
    x1 <- mean(x$score_20)
    ggplot2::ggplot(data = x, aes(x = .data$score_20)) +
      ggplot2::geom_histogram(bins = nrow(x)/2) +
      ggplot2::geom_vline(xintercept = x1, color = "red", size = 1.5) +
      ggplot2::labs(x = "Note des grilles completées [/20] - moyenne en rouge",
           y = "Dénombrement") +
      cowplot::theme_cowplot()
  })

  output$summary_tab <- DT::renderDataTable({
    req(input$correction)
    x <- summary_react()
    x1 <- x[, c("team", "student", "score_20", "missing")]
    x1 <- collapse::roworder(x1, -missing, score_20)

    x1$team[x1$team == x1$student] <- "/"

    DT::formatStyle(
      DT::datatable(x1,
        colnames = c('Equipe', 'Etudiant', 'Note [/20]','Entrées manquantes'),
        rownames = FALSE,
        selection = "single",
        options = list(language = list(search = 'Filtrer :'))
      ),
      "score_20",
      target = "row",
      backgroundColor = DT::styleEqual(NA, "#FF4500")
    )
  })


  # tabItem : correction_grid ----

  output$correction_set2 <- renderInfoBox({correction_set()})

  output$template_link2 <- renderInfoBox({template_link()})

  output$evaluator_name2 <- renderInfoBox({evaluator_name()})

  git_stats <- reactive({
    context <- context_react()
    #message("Git stats file: ", context$git_dir)
    res <- get_git_stats(context$git_stats_file, type = input$contri_ext,
      tz = git_stats_tz)
    #res$author_date <- with_tz(
    #  # TODO: generalize this!
    #  ymd_hms(res$author_date,tz = "UTC"), "Europe/Paris")
    #res$author_date <- ymd_hms(as.character(res$author_date))
    res
  })

  ## selection_grid ----
  selection_grid <- reactiveVal()

  observeEvent(input$correction, {
    res <- NULL
    selection_grid(res)
    message(glue::glue("
      reset selection_grid() : {selection_grid()}"))
  })

  observeEvent(input$summary_tab_rows_selected,{
    # Collect the selected row
    info <- input$summary_tab_rows_selected
    # Use the summary_react table to find the grid
    res <- collapse::roworder(summary_react(), -missing, score_20)
    res1 <- res[info,]
    grid <- paste0(res1$assignment, "-", res1$team)

    # Check that the grid names correspond to the context
    context <- context_react()

    if (!any(context$repos_names %in% grid)) {
      if (any(context$repos_names %in% tolower(grid))) {
        grid <- tolower(grid)
      } else {
        grid <- NULL
      }
    }
    # Update selection_grid()
    selection_grid(grid)
    # debug test
    message(glue::glue("
    #### summary_tab_rows_selected ####
    The selection_grid : {selection_grid()}
    The info : {info} "))
  })

  observeEvent(input$correction_table_criterion_cell_clicked,{
    # Collect the selected cell
    info <- input$correction_table_criterion_cell_clicked
    # Check that the grid names correspond to the context
    context <- context_react()
    grid <- context$repos_names[
        order[info$row]]

    if (!any(context$repos_names %in% grid))
     grid <- NULL

    # Update selection_grid()
    selection_grid(grid)
    # debug test
    message(glue::glue("
    #### correction_table_criterion_cell_clicked ####
    The selection_grid : {selection_grid()}
    The info : row = {info$row}, col =  {info$col}"))
  })

  observe({
    context <- context_react()
    selection1 <- selection_grid()
    updateSelectizeInput(session, 'grid', choices = context$repos_names,
      selected = selection1)
  })

  #output$contri_plot <- plotly::renderPlotly({
  output$contri_plot <- renderPlot({
    context <- context_react()
    x <- git_stats()
    stat_red <- x[tolower(x$github_repository) %in% tolower(input$grid), ]

    if (input$contri_split == "séparer") {
      p <- ggplot2::ggplot(data = stat_red,
        x = .data$author_date, y = .data$change_cum, col = .data$author)) +
        ggplot2::facet_wrap(vars(extension)) +
        ggplot2::geom_step(size = 1, na.rm = TRUE) +
    } else {
      p <- ggplot2::ggplot(data = stat_red,
        aes(x = .data$author_date, y = .data$change_cum, col = .data$author))
    }
    p <- p +
      ggplot2::geom_step(size = 1, na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = "Temps", y = "Somme cumulée des lignes modifiés par projet", color = "Auteur") +
      cowplot::theme_cowplot()

    assign_infos <- context$assign_infos
    assign_time <- c(start = assign_infos$start[1], end = assign_infos$end[1])

    if (length(assign_time) == 2) {
      stat_time <- c(
        start = min(stat_red$author_date),
        end   = max(stat_red$author_date))
      plot_range <- range(c(assign_time,
        min(stat_red$author_date),
        max(stat_red$author_date)))
      p <- p +
        ggplot2::geom_vline(xintercept = assign_time,
          linetype = 4, alpha = 0.8) +
        ggplot2::xlim(plot_range) +
        ggplot2::labs(caption =
        "Chaque point représente un commit. Les lignes verticales représentent le début et la fin de l'exercice.")
    }

    p
    #plotly::ggplotly(p)
  })

  correction_table_grid_react <- reactive({
      req(input$grid)
      context <- context_react()

      populate_table(items = "all", grids = input$grid, context = context,
          reorder = FALSE, highlight = input$highlight, max_lines = max_lines)
    })

  output$correction_table_grid <- DT::renderDataTable({
    req(input$grid)

    DT::formatStyle(
      DT::datatable(
        correction_table_grid_react(),
        colnames = c('Max', 'Score&nbsp;Commentaire', 'Critère',  'Contenu',
          'Graphique', 'Liens', 'Evaluateur', 'Étudiant/groupe'),
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        callback = DT::JS(js),
        extensions = c("Buttons", "KeyTable"),
        options = list(
          keys = TRUE,
          paging = FALSE,
          searching = TRUE,
          #fixedColumns = TRUE,
          autoWidth = TRUE,
          autoFill = TRUE,
          ordering = FALSE,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          language = list(search = 'Filtrer :'),
          columnDefs = list(
            list(width = '300px', targets = 1),  # score_comment
            #list(width = '300px', targets = 2), # criterion
            list(width = '1000px', targets = 3), # content
            list(visible = FALSE, targets = 7)   # student/group (hidden here)
          )
        ),
        editable = list(target = "cell", disable = list(columns = c(0, 2:10))),
        class = "display"
      ), 'score_comment', backgroundColor = "lightgrey"
    )
  })


  # tabItem : correction_criterion ----

  output$correction_set3 <- renderInfoBox({correction_set()})

  output$template_link3 <- renderInfoBox({template_link()})

  output$evaluator_name3 <- renderInfoBox({evaluator_name()})
  ## Select the criterion

  selection_criterion <- reactiveVal()

  observeEvent(input$correction, {
    res <- NULL
    selection_criterion(res)
  })

  observeEvent(input$correction_table_grid_cell_clicked,{
    # Collect the selected cell
    info <- input$correction_table_grid_cell_clicked
    # Check that the criterion correspond to the context
    context <- context_react()

    res <- NULL

    if (!is.null(info$row)) {
     x <- correction_table_grid_react()[info$row, ]
     res <- x$criterion
    }

    if (!any(context$templ_corrs$criterion %in% res))
      res <- NULL
    # Update selection_grid()
    selection_criterion(res)
    # Message info
    message(glue::glue("
    #### correction_table_grid_cell_clicked ####
    The selection_grid : {selection_criterion()}
    The info : row = {info$row}, col =  {info$col}"))
  })

  observe({
    context <- context_react()
    selection1 <- selection_criterion()
    updateSelectizeInput(session, 'item',
      choices = context$templ_corrs$criterion,
      selected = selection1)
  })

  correction_table_criterion_react <- reactive({
    req(input$item)
    context <- context_react()

    sort_table(populate_table(items = input$item, grids = "all",
          context = context, reorder = TRUE, highlight = input$highlight,
          max_lines = max_lines))
  })

  output$correction_table_criterion <- DT::renderDataTable({
    DT::formatStyle(
      DT::datatable(
        correction_table_criterion_react(),
        colnames = c('Max', 'Score&nbsp;Commentaire', 'Critère', 'Contenu',
          'Graphique', 'Liens', 'Evaluateur', 'Étudiant/groupe'),
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        callback = DT::JS(js),
        extensions = c("Buttons", "KeyTable"),
        options = list(
          keys = TRUE,
          paging = FALSE,
          searching = TRUE,
          #fixedColumns = TRUE,
          autoWidth = TRUE,
          autoFill = TRUE,
          ordering = FALSE,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          language = list(search = 'Filtrer :'),
          columnDefs = list(
            list(width = '300px', targets = 1), # score_comment
            list(visible = FALSE, targets = 2), # criterion (don't show it here)
            list(width = '1000px', targets = 3) # content
          )
        ),
        editable = list(target = "cell", disable = list(columns = c(0, 2:10))),
        class = "display"
      ), 'score_comment', backgroundColor = "lightgrey"
    )
  })

  #output$last_edited <- renderText({
  #  #str(input$correction_table_criterion_cell_edit)
  #  if (!is.null(input$correction_table_criterion_cell_edit)) {
  #    if (input$correction_table_criterion_cell_edit$col == 2) {
  #      score_comment <- input$correction_table_criterion_cell_edit$value
  #      score <- trimws(sub("^([^ ]+).*$", "\\1", score_comment))
  #      comment <- trimws(sub("^[^ ]+(.*)$", "\\1", score_comment))
  #      num_score <- try(as.numeric(score), silent = TRUE)
  #      if (inherits(num_score, "try-error")) {
  #        "ERROR: score must be a numeric value or NA"
  #      } else {# TODO: check that score is between 0 and item_score_max
  #        paste(
  #          context$user_logins[
  #            order[input$correction_table_criterion_cell_edit$row]],
  #          "score:", score, "comment:", comment)
  #      }
  #    }
  #  }
  #})
  #TODO: the same for last_edited_grid vs correction_table_grid
  #TODO: add shinyFeedback item, something like
  #half <- reactive({
  #  even <- input$orrectionTable_cell_edit %% 2 == 0
  #  shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
  #  input$orrectionTable_cell_edit / 2
  #})
  # Or use this instead:
  #showNotification("message", type = "message")
  #showNotification("message", type = "warning")
  #showNotification("message", type = "error")

  #myProxy = dataTableProxy('correction_table_criterion')

  observeEvent(input$correction_table_criterion_cell_edit, {
    # validate(need(!is.null(input$correction_table_criterion_cell_edit), ''))
    #print(input$correction_table_criterion_cell_edit$row)
    #print(input$correction_table_criterion_cell_edit$col)
    #print(input$correction_Table_criterion_cell_edit$value)
    if (input$correction_table_criterion_cell_edit$col == 1) {
      # This is a score, possibly followed by a comment
      # Get the corr_file
      context <- context_react()
      corr_file <- context$corr_files[
        order[input$correction_table_criterion_cell_edit$row]]
      # Read it
      message("Changing ", corr_file, " rubric...")
      corrs <- suppressMessages(read.csv(corr_file))
      # Make sure columns score, evaluator and comment are the right type
      corrs$score <- as.numeric(corrs$score)
      corrs$evaluator <- as.character(corrs$evaluator)
      corrs$comment <- as.character(corrs$comment)
      # Get position in the file
      pos <- (1:nrow(corrs))[corrs$criterion == input$item]
      max <- corrs$max[pos]
      if (length(pos) != 1) {
        msg <- paste0("ERROR: criterion ", input$item, " not found in ",
          corr_file)
        message(msg)
        showNotification(msg, type = "error")
        #selectCells(myProxy, selected = NULL)

      } else {# pos found in corrs
        score_comment <- input$correction_table_criterion_cell_edit$value
        # Decompose into score and comment
        score <- trimws(sub("^([^ ]+).*$", "\\1", score_comment))
        if (is.null(score) || !length(score) || score == "")
          score <- "NA"
        comment <- trimws(sub("^[^ ]+(.*)$", "\\1", score_comment))
        if (is.null(comment) || !length(comment))
          comment <- ""

        is_ok <- TRUE
        # Special case for score being "NA"
        if (score == "NA") {
          num_score <- as.numeric(NA)
        } else {
          num_score <- try(suppressWarnings(as.numeric(score)), silent = TRUE)
          if (is.na(num_score) || inherits(num_score, "try-error"))
            is_ok <- FALSE
        }

        if (is_ok) {
          # Check score is not > max (note: since we allow negative scores, they
          # can be lower that zero and it is not an error)
          # Exception: if the item is for a BONUS, max is zero, and we allow for
          # positive values
          if (!is.na(num_score) && max > 0 && num_score > max) {
            msg <- paste0("ERROR: attempting to put a score ", num_score,
              " higher than the max score (", max, ")")
            message(msg)
            showNotification(msg, type = "error")
            #selectCells(myProxy, selected = NULL)

          } else {# Everything is fine, record this entry
            corrs[pos, "score"] <- num_score
            corrs[pos, "comment"] <- comment
            corrs[pos, "evaluator"] <- input$evaluator
            data.io::write$csv(corrs, corr_file)
          }
        } else {# !is_ok
          msg <- paste0("ERROR: score '", score,
          "' not convertible into a numeric value")
          message(msg)
          showNotification(msg, type = "error")
          #selectCells(myProxy, selected = NULL)
        }
      }
    }
  })

  # TODO: refactor to avoid duplicated code
  observeEvent(input$correction_table_grid_cell_edit, {
    # validate(need(!is.null(input$correction_table_grid_cell_edit), ''))
    #print(input$correction_table_grid_cell_edit$row)
    #print(input$correction_table_grid_cell_edit$col)
    #print(input$correction_Table_grid_cell_edit$value)
    if (input$correction_table_grid_cell_edit$col == 1) {
      # This is a score, possibly followed by a comment
      # Get the corr_file
      context <- context_react()
      corr_pos <-
        (1:length(context$repos_names))[context$repos_names == input$grid]
      if (length(corr_pos) != 1) {
        msg <- paste0("ERROR: grid ", input$grid, " not found")
        message(msg)
        showNotification(msg, type = "error")
        #selectCells(myProxy, selected = NULL)
      } else {
        corr_file <- context$corr_files[corr_pos]
        message("Changing ", corr_file, " rubric...")
        # Read it
        corrs <- suppressMessages(read.csv(corr_file))
        # Make sure columns score, evaluator and comment are the right type
        corrs$score <- as.numeric(corrs$score)
        corrs$evaluator <- as.character(corrs$evaluator)
        corrs$comment <- as.character(corrs$comment)
        # Get position in the file (criterion)... assume no changes in the order
        # of the criteria in the evaluation grid!
        pos <- input$correction_table_grid_cell_edit$row
        max <- corrs$max[pos]
        score_comment <- input$correction_table_grid_cell_edit$value
        # Decompose into score and comment
        score <- trimws(sub("^([^ ]+).*$", "\\1", score_comment))
        if (is.null(score) || !length(score) || score == "")
          score <- "NA"
        comment <- trimws(sub("^[^ ]+(.*)$", "\\1", score_comment))
        if (is.null(comment) || !length(comment))
          comment <- ""

        is_ok <- TRUE
        # Special case for score being "NA"
        if (score == "NA") {
          num_score <- as.numeric(NA)
        } else {
          num_score <- try(suppressWarnings(as.numeric(score)), silent = TRUE)
          if (is.na(num_score) || inherits(num_score, "try-error"))
            is_ok <- FALSE
        }

        if (is_ok) {
          # Check score is not > max (note: since we allow negative scores, they
          # can be lower that zero and it is not an error)
          # Exception: if the item is for a BONUS, max is zero, and we allow for
          # positive values
          if (!is.na(num_score) && max > 0 && num_score > max) {
            msg <- paste0("ERROR: attempting to put a score ", num_score,
              " higher than the max score (", max, ")")
            message(msg)
            showNotification(msg, type = "error")
            #selectCells(myProxy, selected = NULL)

          } else {# Everything is fine, record this entry
            corrs[pos, "score"] <- num_score
            corrs[pos, "comment"] <- comment
            corrs[pos, "evaluator"] <- input$evaluator
            write.csv(corrs, corr_file, row.names = FALSE)
          }
        } else {# !is_ok
          msg <- paste0("ERROR: score '", score,
            "' not convertible into a numeric value")
          message(msg)
          showNotification(msg, type = "error")
          #selectCells(myProxy, selected = NULL)
        }
      }
    }
  })

  ## Reset last selected value
  #observeEvent(input$reset, {
  #  selectCells(myProxy, selected = NULL)
  #})
}

shinyApp(shinyUI, shinyServer)
