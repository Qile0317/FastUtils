#' Suppress Messages and Output
#'
#' This function suppresses messages and captures output from an expression.
#' Note that this does NOT return the result of the expression! It is
#' intended to be used in contexts where certain operations with side
#' effects are performed but the verbosity should be suppressed.
#'
#' @param e An expression to evaluate.
#'
#' @return The result of the expression with messages suppressed and output
#' captured.
#' @export
#' @keywords interactivity
#' @examples
#'
#' quietly(print(1))
#'
#' quietly({
#'     print(1)
#'     print(2)
#'     print(3)
#' })
#'
#' a <- 1
#' quietly({
#'     a <- a + 1
#'     print(a)
#' })
#' print(a)
#' #> 2
#'
quietly <- function(e) {
    suppressMessages(invisible(utils::capture.output(e)))
}
