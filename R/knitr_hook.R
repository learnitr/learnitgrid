# Functions for Quarto and R Markdown documents ---------------------------

# A hook to save the result of evaluations in chunks as .Last.chunk (if printed
# because things returned invisibly are not recorded)
#' Configure Knitr to generate a .Last.chunk object
#'
#' @description
#' Change the Knitr renderer so that the last computed object is automatically
#' saved as `.Last.chunk`, and to record results from a chunk with chunk options
#' `record=`.
#'
#' @return The `opts_chunk` or the `knit_hook` set accordingly.
#' @export
hook_last_chunk <- function() {
  knitr::opts_chunk$set(render = function(x, ...){
    svMisc::assign_temp(".Last.chunk", x, replace.existing = TRUE)
    knitr::knit_print(x, ...)
  })
}

hook_record <- function() {
  # A hook to save results after a code chunk is evaluated
  knitr::knit_hooks$set(record = function(before, options, envir) {
    if (!before) {
      fun_name <- options$record
      fun <- get(fun_name, mode = "function", envir = envir)
      object <- options$object
      if (is.null(object)) {
        # If the function name starts with RN, we use .Last.chunk
        # otherwise, we use same name as label
        if (substring(fun_name, 1L, 2L) == "RN") {
          object <- ".Last.chunk"
        } else {
          object <- options$label
        }
        cat(fun_name, "('", options$label, "')\n", sep = "")
      } else {
        object <- options$object
        cat(fun_name, "('", object, "', '", options$label, "')\n", sep = "")
      }
      procfun <- options$fun
      if (!is.null(procfun) & !is.function(procfun))
        procfun <- get(procfun, mode = "function", envir = envir)
      arg <- options$arg
      if (is.null(arg)) {
        if (is.null(procfun)) {
          fun(object_name = object, name = options$label, env = envir)
        } else {
          fun(object_name = object, name = options$label,
            fun = procfun, env = envir)
        }
      } else {# There is an extra argument
        if (is.null(procfun)) {
          fun(object_name = object, name = options$label, arg, env = envir)
        } else {
          fun(object_name = object, name = options$label, arg,
            fun = procfun, env = envir)
        }
      }
      NULL
    }
  })
}
