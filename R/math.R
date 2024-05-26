#' Bound a Number within a Range
#'
#' This function bounds a number within a specified range.
#'
#' @param num A numeric vector to be bounded.
#' @param lowerbound The lower bound of the range.
#' @param upperbound The upper bound of the range.
#' 
#' @return A numeric vector with elements bounded within the specified range.
#' @export
#' @keywords math
#' 
#' @examples
#' bound(1, 0, 2)
#' bound(1:10, -1, 5)
#' 
bound <- function(num, lowerbound, upperbound) {
    sapply(num, function(x) min(max(x, lowerbound), upperbound))
}

#' Check if a Number is within a Range
#'
#' This function checks if a number is within a specified range.
#'
#' @param num A numeric vector to be checked.
#' @param lowerbound The lower bound of the range.
#' @param upperbound The upper bound of the range.
#'
#' @return A logical vector indicating whether each element is within the specified range.
#' @export
#' @keywords math
#' 
#' @examples
#' isBound(1, 0, 2)
#' isBound(1:10, -1, 5)
#' 
isBound <- function(num, lowerbound, upperbound) {
    (num >= lowerbound) & (num <= upperbound)
}

#' Add Two Objects
#'
#' This function adds two objects. If both objects are numeric vectors,
#' it performs element-wise addition. If one or both objects are strings,
#' it concatenates them. For other objects, it attempts to use the `+`
#' method defined for the class of the objects.
#'
#' @param x An object.
#' @param y An object.
#' 
#' @return The result of adding the two objects.
#' @export
#' @keywords math
#' 
#' @examples
#' # Add two numeric vectors
#' add(c(1, 2, 3), c(4, 5, 6))
#' # Concatenate two strings
#' add("hello", "world")
#' # Add a number and a string (concatenation)
#' add(1, " world")
#' 
add <- function(x, y) {
  if (is.character(x) || is.character(y)) {
    paste(x, y, sep = "")
  } else {
    x + y
  }
}

#' Subtract Two Numbers
#'
#' This function subtracts one number from another.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' 
#' @return A numeric vector representing the difference between the input vectors.
#' @export
#' @keywords math
#' @examples
#' # Subtract two numeric vectors
#' subtract(c(10, 20, 30), c(1, 2, 3))
subtract <- function(x, y) x - y

#' Multiply Two Numbers
#'
#' This function multiplies two numbers.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' 
#' @return A numeric vector representing the product of the input vectors.
#' @export
#' @keywords math
#' @examples
#' # Multiply two numeric vectors
#' multiply(c(2, 3, 4), c(5, 6, 7))
multiply <- function(x, y) x * y

#' Divide Two Numbers
#'
#' This function divides one number by another.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' 
#' @return A numeric vector representing the quotient of the input vectors.
#' @export
#' @keywords math
#' @examples
#' # Divide two numeric vectors
#' divide(c(10, 20, 30), c(2, 4, 5))
divide <- function(x, y) x / y

#' Check if a Number is Even
#'
#' This function checks if a number is even.
#'
#' @param x A numeric vector.
#' 
#' @return A logical vector indicating whether each element is even.
#' @export
#' @keywords math
#' @examples
#' # Check if numbers are even
#' isEven(c(1, 2, 3, 4))
isEven <- function(x) x %% 2 == 0

#' Check if a Number is Odd
#'
#' This function checks if a number is odd.
#'
#' @param x A numeric vector.
#' 
#' @return A logical vector indicating whether each element is odd.
#' @export
#' @keywords math
#' @examples
#' # Check if numbers are odd
#' isOdd(c(1, 2, 3, 4))
isOdd <- function(x) x %% 2 == 1
