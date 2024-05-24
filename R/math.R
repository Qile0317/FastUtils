#' @export
bound <- function(num, lowerbound, upperbound) {
    sapply(num, function(x) min(max(x, lowerbound), upperbound))
}

#' @export
isBound <- function(num, lowerbound, upperbound) {
    (num >= lowerbound) & (num <= upperbound)
}

#' @export
add <- function(x, y) x + y

#' @export
subtract <- function(x, y) x - y

#' @export
multiply <- function(x, y) x * y

#' @export
divide <- function(x, y) x / y

#' @export
isEven <- function(x) x %% 2 == 0

#' @export
isOdd <- function(x) x %% 2 == 1
