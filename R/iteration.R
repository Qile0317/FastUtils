#' Generate Unique Pairs Up To a Number
#'
#' This function generates all unique pairs of integers up to a given number.
#'
#' @param x An integer specifying the upper limit for pairs.
#' @param oneIndexed A logical indicating whether the pairs should be one-indexed. Default is TRUE.
#' 
#' @return A list of unique pairs of integers up to the specified number.
#' @export
#' @keywords iteration
getUniquePairsUpTo <- function(x, oneIndexed = TRUE) {
    if (x <= 1) return(list())
    rcppGetUniquePairsUpTo(as.integer(x), oneIndexed = oneIndexed)
}

#' Zip Multiple Vectors or Lists
#'
#' This function combines multiple vectors or lists element-wise into a list of lists.
#'
#' @param ... Vectors or lists to be combined.
#' 
#' @return A list of lists, where each inner list contains the elements from the corresponding positions in the input vectors or lists.
#' @export
#' @keywords iteration
zip <- function(...) mapply(list, ..., SIMPLIFY = FALSE)

#' Enumerate Elements with Indices
#'
#' This function pairs elements of vectors or lists with their indices.
#'
#' @param ... Vectors or lists to be enumerated.
#' @param zero_indexed A logical indicating whether indexing should start from zero. Default is FALSE.
#' 
#' @return A list of lists, where each inner list contains an index and the corresponding elements from the input vectors or lists.
#' @export
#' @keywords iteration
enumerate <- function(..., zero_indexed = FALSE) {
    zip(seq_along(..1) - zero_indexed, ...)
}

#' Get Index from Enumerated Element
#'
#' This function extracts the index from an enumerated element.
#'
#' @param elem An enumerated element.
#' 
#' @return The index of the enumerated element.
#' @export
#' @keywords iteration
ind <- function(elem) elem[[1]]

#' Get Value from Enumerated Element by Index
#'
#' This function extracts the value from an enumerated element by the given index.
#'
#' @param elem An enumerated element.
#' @param index The index of the value to extract.
#' 
#' @return The value at the specified index in the enumerated element.
#' @export
#' @keywords iteration
val <- function(elem, index) elem[[index + 1]]

#' Get First Value from Enumerated Element
#'
#' This function extracts the first value from an enumerated element.
#'
#' @param elem An enumerated element.
#' 
#' @return The first value in the enumerated element.
#' @export
#' @keywords iteration
val1 <- function(elem) val(elem, 1)
