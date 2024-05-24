#' @export
getUniquePairsUpTo <- function(x, oneIndexed = TRUE) {
    if (x <= 1) return(list())
    rcppGetUniquePairsUpTo(as.integer(x), oneIndexed = oneIndexed)
}

#' @export
zip <- function(...) mapply(list, ..., SIMPLIFY = FALSE)

#' @export
enumerate <- function(..., zero_indexed = FALSE) {
    zip(seq_along(..1) - zero_indexed, ...)
}

#' @export
ind <- function(elem) elem[[1]]

#' @export
val <- function(elem, index) elem[[index + 1]]

#' @export
val1 <- function(elem) val(elem, 1)
