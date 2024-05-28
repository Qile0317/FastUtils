#' Zip Multiple Vectors or Lists
#'
#' This function combines multiple vectors or lists element-wise into a list of lists.
#'
#' @param ... Vectors or lists to be combined.
#' 
#' @return A list of lists, where each inner list contains the elements from the corresponding positions in the input vectors or lists.
#' @export
#' @keywords iteration
#' @examples
#' # Zip two vectors
#' zip(c(1, 2, 3), c("a", "b", "c"))
#' # Zip three vectors
#' zip(c(1, 2), c("x", "y"), c(TRUE, FALSE))
zip <- function(...) mapply(list, ..., SIMPLIFY = FALSE)

#' Enumerate Elements with Indices
#'
#' This function pairs elements of vectors or lists with their indices. The output
#' is meant to be used in a for loop, and each element extracted with the 
#' [ind()], [val()], or [val1()] functions.
#'
#' @param ... Vectors or lists to be enumerated.
#' @param zero_indexed A logical indicating whether indexing should start from zero. Default is FALSE.
#' 
#' @return A list of lists, where each inner list contains an index and the corresponding elements from the input vectors or lists.
#' @export
#' @keywords iteration
#' 
#' @seealso [ind()], [val()], [val1()]
#' 
#' @examples
#' # Enumerate a vector
#' enumerate(c("a", "b", "c"))
#' # Enumerate a vector starting from zero
#' enumerate(c("a", "b", "c"), zero_indexed = TRUE)
#' # Enumerate two vectors
#' enumerate(c(1, 2), c("x", "y"))
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
#' @examples
#' # Extract index from an enumerated element
#' elem <- list(1, "a")
#' ind(elem)
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
#' @examples
#' # Extract value from an enumerated element by index
#' elem <- list(1, "a", "b")
#' val(elem, 2)
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
#' @examples
#' # Extract the first value from an enumerated element
#' elem <- list(1, "a", "b")
#' val1(elem)
val1 <- function(elem) val(elem, 1)
