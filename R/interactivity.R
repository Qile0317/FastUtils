#' Suppress Messages and Output
#'
#' This function suppresses messages and captures output from an expression.
#'
#' @param e An expression to evaluate.
#' 
#' @return The result of the expression with messages suppressed and output captured.
#' @export
#' @keywords interactivity
quietly <- function(e) suppressMessages(capture.output(e))
