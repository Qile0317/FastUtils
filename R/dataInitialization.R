#' Initialize a Vector
#'
#' This function initializes a vector based on a specified type and size, with an optional initial value.
#'
#' @param typeFunc A character string indicating the type of the vector or a function to create the vector.
#' @param x The length of the vector.
#' @param initVal An optional initial value to fill the vector.
#' 
#' @return A vector of the specified type and size, optionally initialized with a value.
#' @export
#' @keywords dataInitialization
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
#' This function initializes a list based on size or names, with an optional initial value.
#'
#' @param x A character vector as names, or an numeric indicating the size of the list.
#' @param initVal an optional initial value for all elements of the list.
#'
#' @return A list of the specified size and names, optionally initialized with a value.
#' @export
#' @keywords dataInitialization
initList <- function(x, initVal = NULL) {

    if (is.character(x)) {
        lst <- structure(vector("list", length(x)), names = x)
    } else {
        lst <- vector("list", as.integer(x))
    }

    lapply(lst, function(x) initVal)
}

#' Initialize an Empty Table
#'
#' This function initializes an empty table.
#'
#' @return An empty table structure.
#' @export
#' @keywords dataInitialization
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
namedNumericToTable <- function(x) {
    output <- as.integer(x)
    names(output) <- names(x)
    output <- as.table(output)
    names(dimnames(output)) <- ""
    output
}

#' Create a Hash Table
#'
#' This function creates a hash table from a set of keys and optional initial value.
#'
#' @param keys A vector of keys for the hash table.
#' @param init_vals Optional initial value for the hash table.
#'
#' @return A hash table with the specified keys and initial values.
#' @export
#' @keywords dataInitialization
createHash <- function(keys, init_vals = NULL) {
    if (missing(keys)) return(hash::hash())
    keys <- unique(keys)
    numkeys <- length(keys)
    switch(as.character(numkeys),
        "0" = hash::hash(),
        "1" = hash::hash(keys, init_vals),
        hash::hash(keys, initList(numkeys, init_vals))
    )
}
