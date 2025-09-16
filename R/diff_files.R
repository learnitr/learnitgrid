#' Launch a Shiny application to compare original and solution files
#'
#' This function starts a Shiny app that allows users to select an "_original" file
#' and automatically compares it with its corresponding "_solution" file using the `diffr` package.
#'
#' @return Launches a Shiny application.
#' @examples
#' #diff_files()
#' @export
diff_files <- function() {
  ui <- shiny::fluidPage(
    shiny::h1("Comparison between original and solution files"),
    shiny::selectInput("file1", "Select the original file:", choices = NULL),
    diffr::diffrOutput("exdiff")
  )

  server <- function(input, output, session) {
    folder_path <- rstudioapi::getActiveProject()

    shiny::observe({
      if (dir.exists(folder_path)) {
        files_orig <- list.files(
          folder_path,
          pattern = "_original.*",
          full.names = FALSE,
          recursive = TRUE,
          all.files = TRUE
        )
        shiny::updateSelectInput(session, "file1", choices = files_orig)
      } else {
        shiny::showNotification("The project folder does not exist.", type = "error")
      }
    })

    output$exdiff <- diffr::renderDiffr({
      shiny::req(input$file1)
      file2 <- gsub("_original", "_solution", input$file1)

      file1_path <- file.path(folder_path, input$file1)
      file2_path <- file.path(folder_path, file2)

      if (!file.exists(file2_path)) {
        shiny::showNotification("The corresponding solution file does not exist or is not accessible.", type = "error")
        return(NULL)
      }

      diffr::diffr(file1_path, file2_path,
                   before = input$file1, after = file2)
    })
  }

  shiny::shinyApp(ui, server)
}
