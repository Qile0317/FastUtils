#' Get the Last Elements of a Vector or List
#'
#' This function retrieves the last `n` elements of a vector or list.
#'
#' @param x A vector, list, or other supported data type.
#' @param n An integer specifying the number of elements to retrieve from the end. Default is 1.
#' 
#' @return The last `n` elements of the input.
#' @export
#' @keywords indexing
getlast <- function(x, n = 1) {
    UseMethod("getlast")
}

#' @export
getlast.default <- function(x, n = 1) {
    index <- length(x) - n + 1
    if (is.list(x)) {
        return(x[[index]])
    }
    x[index]
}

#' Get the First Elements of a Vector or List
#'
#' This function retrieves the first `n` elements of a vector or list.
#'
#' @param x A vector, list, or other supported data type.
#' @param n An integer specifying the number of elements to retrieve from the start. Default is 1.
#' 
#' @return The first `n` elements of the input.
#' @export
#' @keywords indexing
getfirst <- function(x, n = 1) {
    UseMethod("getfirst")
}

#' @export
getfirst.default <- function(x, n = 1) {
    index <- n
    if (is.list(x)) {
        return(x[[index]])
    }
    x[index]
}
