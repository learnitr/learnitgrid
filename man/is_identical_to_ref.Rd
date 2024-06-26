% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_functions.R
\name{is_identical_to_ref}
\alias{is_identical_to_ref}
\alias{is_equal_to_ref}
\alias{is_rendered}
\alias{is_rendered_current}
\alias{is_data}
\alias{is_data_df}
\alias{has_labels_all}
\alias{has_labels_any}
\alias{has_units_all}
\alias{has_units_any}
\alias{is_display_equation}
\alias{is_display_param_equation}
\alias{is_inline_equation}
\alias{is_inline_param_equation}
\title{Functions to be used in {testthat} test of the project}
\usage{
is_identical_to_ref(name, part = NULL, attr = NULL)

is_equal_to_ref(name, part = NULL, attr = NULL)

is_rendered(quarto, format = "html")

is_rendered_current(quarto, format = "html")

is_data(name, dir = "data", format = "rds", check_df = FALSE)

is_data_df(name, dir = "data", format = "rds", check_df = TRUE)

has_labels_all(name, part = NULL)

has_labels_any(name, part = NULL)

has_units_all(name, part = NULL)

has_units_any(name, part = NULL)

is_display_equation(text, object)

is_display_param_equation(text, object)

is_inline_equation(text, object)

is_inline_param_equation(text, object)
}
\arguments{
\item{name}{The name of the result and reference files}

\item{part}{The part(s) (list item(s)) to co=pare}

\item{attr}{The attribute(s) to compare}

\item{quarto}{The name of the Quarto or R Markdown file}

\item{format}{The format of the rendered file. For \code{is_rendered()} or \code{is_rendered_current()} it is \verb{"html'} by default. For the other function, the default is \code{"rds"}}

\item{dir}{The subdirectory in the package where to look for (\code{"data"} by default)}

\item{check_df}{Check if the data file contains a data frame}

\item{text}{The text to check}

\item{object}{The object to look for in the \code{eq__()} function}
}
\value{
\code{TRUE} if the result is identical to the reference, \code{FALSE} otherwise
}
\description{
Functions to be used in {testthat} test of the project
}
