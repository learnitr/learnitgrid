% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/populate_table.R
\name{populate_table}
\alias{populate_table}
\title{Computes the content of a DT::datatable for a correction set}
\usage{
populate_table(
  items,
  grids = "all",
  context,
  reorder = (length(items) == 1),
  highlight = FALSE,
  max_lines = 30L,
  on_github = TRUE
)
}
\arguments{
\item{items}{The items of the evaluation grid to display in the table,
usually either one item, or "all" for everything}

\item{grids}{The evaluation grids to display, usually either "all" if only
one item, or one grid if "all" items}

\item{context}{A context object as computed by \code{\link[=create_context]{create_context()}}.}

\item{reorder}{Should the rows in the table be reordered by similarities?
This is usually \code{TRUE} for a single items, or \code{FALSE} otherwise (and it is
computed as such by default).}

\item{highlight}{Syntax highlighting for code (slow, thus \code{FALSE} by default)}

\item{max_lines}{The maximum number of content lines that are displayed
(truncate very long contents).}

\item{on_github}{Should the links point to the GitHub repository or to the
local files (default is \code{TRUE})?}
}
\value{
A data frame with the content to be displayed in a DT::datatable object.
}
\description{
Populate a DT table according to selected items with a list of grids (or
"all") or according to a selected grid with a list of items (or "all")
}
