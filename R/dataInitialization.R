#' Initialize a Vector
#'
#' This function initializes a vector based on a specified type and size, with
#' an optional initial value.
#'
#' @param typeFunc A character string indicating the type of the vector or a
#' function to create the vector.
#' @param x The length of the vector.
#' @param initVal An optional initial value to fill the vector.
#'
#' @return A vector of the specified type and size, optionally initialized
#' with a value.
#' @export
#' @keywords dataInitialization
#' @examples
#' # Create a numeric vector of length 5
#' initV("numeric", 5)
#' # Create a logical vector of length 3 initialized with TRUE
#' initV("logical", 3, TRUE)
initV <- function(typeFunc, x, initVal = NULL) {
    if (is(typeFunc, "character")) {
        v <- vector(typeFunc, x)
    } else {
        v <- typeFunc(x)
    }
    if (is.null(initVal)) return(v)
    sapply(v, function(i) initVal)
}

#' Initialize a List
#'
#' This function initializes a list based on size or names, with an optional
#' initial value.
#'
#' @param x A character vector as names, or an numeric indicating the size of
#' the list.
#' @param initVal an optional initial value for all elements of the list.
#'
#' @return A list of the specified size and names, optionally initialized with
#' a value.
#' @export
#' @keywords dataInitialization
#' @examples
#' # Create a list with 3 elements
#' initList(3)
#' # Create a named list initialized with NULL
#' initList(c("a", "b", "c"))
#' # Create a list with 2 elements initialized with 0
#' initList(2, 0)
initList <- function(x = NULL, initVal = NULL) {

    if (length(x) == 0) return(list())

    if (is.character(x)) {
        lst <- structure(vector("list", length(x)), names = x)
    } else {
        lst <- vector("list", as.integer(x))
    }

    lapply(lst, function(x) initVal)
}

#' Initialize a DataFrame with Column Names
#'
#' This function creates an empty data frame and assigns the specified column
#' names with zero rows.
#'
#' @param colnames A character vector specifying the names of the columns for
#' the data frame. This vector will be attempted to be coerced to a character.
#'
#' @return A data frame with the specified column names. Non unique names will
#' be handled by the conventions of [data.frame()].
#' prefixes.
#' @export
#' @keywords dataInitialization
#' @examples
#' # Create a data frame with specified column names initialized with NA
#' initDataFrameWithColnames(c("Name", "Age", "Gender"))
#'
initDataFrameWithColnames <- function(colnames = NULL) {
    if (length(colnames) == 0) return(data.frame())
    dplyr::slice(data.frame(initList(as.character(colnames), NA)), 0)
}

#' Initialize an Empty Table
#'
#' This function initializes an empty table.
#'
#' @return An empty table structure.
#' @export
#' @keywords dataInitialization
#' @examples
#' # Create an empty table
#' initEmptyTable()
initEmptyTable <- function() {
    structure(
        integer(0),
        dim = 0L,
        dimnames = structure(list(NULL), names = ""),
        class = "table"
    )
}

#' Convert a Table to Numeric
#'
#' This function converts a table to a numeric vector.
#'
#' @param x A table to be converted.
#'
#' @return A numeric vector with names preserved from the table.
#' @export
#' @keywords dataInitialization
#' @examples
#' # Convert a table to numeric
#' tbl <- table(c("a", "b", "a"))
#' tableToNumeric(tbl)
tableToNumeric <- function(x) {
    structure(as.numeric(x), names = names(x))
}

#' Convert Named Numeric Vector to Table
#'
#' This function converts a named numeric vector to a table.
#'
#' @param x A named numeric vector.
#'
#' @return A table with the same names and values as the input vector.
#' @export
#' @keywords dataInitialization
#' @examples
#' # Convert a named numeric vector to a table
#' vec <- c(a = 1, b = 2, c = 3)
#' namedNumericToTable(vec)
namedNumericToTable <- function(x) {
    assertthat::assert_that(is.numeric(x) && !is.null(names(x)))
    output <- as.integer(x)
    names(output) <- names(x)
    output <- as.table(output)
    names(dimnames(output)) <- ""
    output
}

#' Create a Hash Table
#'
#' This function creates a hash table from a set of keys and optional initial
#' value. Note that it is simply a convenience wrapper for the `hash` package.
#'
#' @param keys A vector of keys for the hash table.
#' @param initVals Optional initial value for the hash table.
#'
#' @return A hash table with the specified keys and initial values.
#' @export
#' @keywords dataInitialization
#' @examples
#' # Create a hash table with keys and no initial values
#' createHash(c("a", "b", "c"))
#' # Create a hash table with keys and initial value of 0
#' createHash(c("a", "b", "c"), 0)
createHash <- function(keys, initVals = NULL) {

    if (missing(keys)) return(hash::hash())
    assertthat::assert_that(is.character(keys))
    assertthat::assert_that(is.null(initVals) || length(initVals) == 1)

    keys <- unique(keys)
    numkeys <- length(keys)
    switch(as.character(numkeys),
        "0" = hash::hash(),
        "1" = hash::hash(keys, initVals),
        hash::hash(keys, initList(numkeys, initVals))
    )
}
