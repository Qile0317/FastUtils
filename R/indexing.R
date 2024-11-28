#' Get the Last Elements of a Vector or List
#'
#' This function retrieves the last `n` elements of a vector or list.
#'
#' @param x A vector, list, or other supported data type.
#' @param n An integer specifying the number of elements to retrieve from the
#' end. Default is 1.
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
#' @param n An integer specifying the number of elements to retrieve from the
#' start. Default is 1.
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

#' Set values in an object using indexing
#'
#' Chainable functional alias for `x[i, j] <- value`. It provides flexibility
#' for setting values using either one or two indices. Note that if you only
#' want to modify columns (`j`), the argument along with the `value` must be
#' named. Alternatively use [setCol]. if neither `i` nor `j` is provided, an
#' error is thrown.
#'
#' @param x An object that supports the `"[<-"` method, such as a matrix or
#' data frame.
#' @param i Row index
#' @param j Column index
#' @param value The value to assign to the specified index/indices.
#'
#' @return The modified object.
#' @keywords indexing
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' setIndex(mat, 1, 2, 10)  # Set value at position (1, 2)
#' setIndex(mat, 1, value = 20)  # Set entire first row to 20
#' setIndex(mat, j = 3, value = 30)  # Set entire third column to 30
#' @seealso [setCol()], [setRow()]
#' @export
setIndex <- function(x, i, j, value) {

    assert_that(hasMethod("[<-", class(x)))

    if (missing(i) && missing(j)) {
        stop("Please provide either i and/or j.")
    }

    if (!missing(i) && !missing(j)) {
        x[i, j] <- value
    } else if (!missing(i)) {
        x[i, ] <- value
    } else {
        x[, j] <- value
    }

    x
}

#' Set values in a column of an object
#'
#' Chainable functional alias of `x[, j] <- value`.
#'
#' @inheritParams setIndex
#'
#' @return The modified object.
#' @keywords indexing
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' setCol(mat, j = 2, value = 5)  # Set entire second column to 5
#' @seealso [setIndex()]
#' @export
setCol <- function(x, j, value) {
    x[, j] <- value
    x
}

#' Set values in a row of an object
#'
#' Chainable functional alias of `x[i, ] <- value`.
#'
#' @inheritParams setIndex
#'
#' @return The modified object.
#' @keywords indexing
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' setRow(mat, i = 1, value = 10)  # Set entire first row to 10
#' @seealso [setIndex()]
#' @export
setRow <- function(x, i, value) {
    x[i, ] <- value
    x
}

#' Set a value at a specified index in an object
#'
#' Chainable functional alias of `x[i] <- value`.
#'
#' @param x An object to modify.
#' @param i The index at which the value should be set.
#' @param value The value to assign at the specified index.
#'
#' @return The modified object, with the value set at the specified index.
#' @keywords indexing
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' setAt(x, 2, 10)
#' @export
setAt <- function(x, i, value) {
    x[i] <- value
    x
}
