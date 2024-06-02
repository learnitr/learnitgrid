#' @details
#' The learnitgrid package allows to create and manage semi-automatically
#' rubrics to assess GitHub projects (R scripts, R Markdown or Quarto files).
#' Create directed projects where students have to complete documents and submit
#' them to GitHub (classroom) so that they are evaluated using the rubric
#' (or assessment grid).
#'
#' @section Important functions:
#'
#' - [install_grid_example()] decompress an example dataset.
#'
#' - [run_grid()] run the learnitgrid Shiny application.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom cowplot theme_cowplot
#' @importFrom gh gh gh_whoami
#' @importFrom glue glue
#' @importFrom fs path dir_create dir_exists dir_ls file_exists link_create link_delete link_exists
#' @importFrom collapse fcumsum GRP roworder
#' @importFrom ggplot2 geom_histogram element_text facet_wrap geom_point geom_step geom_vline labs theme xlim
#' @importFrom stats fivenum
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom parsermd as_document by_section has_label has_type parse_rmd rmd_select
#' @importFrom DT datatable dataTableOutput formatStyle JS renderDataTable styleEqual
#' @importFrom flashClust hclust
#' @importFrom shiny shinyApp runApp
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage dashboardSidebar
#' @importFrom shinycssloaders withSpinner
#' @importFrom stringdist stringsim
#' @importFrom utils browseURL read.csv untar write.csv
#' @importFrom writexl write_xlsx
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
