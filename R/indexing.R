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
#' @examples
#' # Get the last element of a vector
#' getlast(c(1, 2, 3, 4, 5))
#' # Get the last 2 elements of a vector
#' getlast(c(1, 2, 3, 4, 5), 2)
#' # Get the last element of a list
#' getlast(list("a", "b", "c"))
#' # Get the last 2 elements of a list
#' getlast(list("a", "b", "c"), 2)
getlast <- function(x, n = 1) {
    UseMethod("getlast")
}

#' @rdname getlast
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
#' @examples
#' # Get the first element of a vector
#' getfirst(c(1, 2, 3, 4, 5))
#' # Get the first 2 elements of a vector
#' getfirst(c(1, 2, 3, 4, 5), 2)
#' # Get the first element of a list
#' getfirst(list("a", "b", "c"))
#' # Get the first 2 elements of a list
#' getfirst(list("a", "b", "c"), 2)
getfirst <- function(x, n = 1) {
    UseMethod("getfirst")
}

#' @rdname getfirst
#' @export
getfirst.default <- function(x, n = 1) {
    index <- n
    if (is.list(x)) {
        return(x[[index]])
    }
    x[index]
}
