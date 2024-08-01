#' evaluates a string as R code, and stops if an error occurs
#'
#' This function evaluates a string as R code, and stops if an error occurs.
#' This can be useful for evaluating code that is generated dynamically.
#'
#' @param ... the R code to evaluate as characters. Will be joined when
#' evaluating.
#' @param envir the environment in which to evaluate the code. Defaults to the
#' parent frame of the function.
#'
#' @return the result of the evaluation
#' @export
#' @keywords metaProgramming
#' @examples
#' # Set names of a vector
#' x <- 1:3
#' x <- evalText("setNames(x, c('A', 'B', 'C'))")
#' x
#'
evalText <- function(..., envir = parent.frame()) {

    input <- paste0(unlist(list(...), use.names = FALSE), collapse = "")

    tryCatch(
        eval(str2lang(input), envir = envir),
        error = function(e) {
            stop(
                "Error in evaluating: `",
                input, "`: ", getFailStr(e)
            )
        }
    )
}
