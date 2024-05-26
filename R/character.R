#' Extract Substring from Start to End Difference
#'
#' This function extracts a substring from a given start position to the position determined by subtracting `endDiff` from the string length.
#'
#' @param x A character string from which the substring is extracted.
#' @param start The starting position for the substring extraction.
#' @param endDiff The difference to subtract from the string length to determine the end position.
#'
#' @return A substring of the input character string.
#' @export
#' @keywords character
#' 
#' @seealso [substr()]
#'
#' @examples
#' substrEnd("12345", 1, 1)
#' substrEnd("12345", 1, 2)
#' substrEnd("12345", 2, 3)
#' 
substrEnd <- function(x, start, endDiff) {
    substr(x, start, nchar(x) - endDiff)
}

#' Enclose String with Specified Characters
#'
#' This function encloses a string with specified characters on the left and the right.
#'
#' @param x A character string to enclose.
#' @param left A character string to prepend.
#' @param right A character string to append.
#'
#' @return A new character string with `x` enclosed by `left` and `right`.
#' @export
#' @keywords character
#' @examples
#' enclose("text", "[", "]") # returns "[text]"
enclose <- function(x, left, right) {
    paste(left, x, right, sep = "")
}

#' Enclose String with Brackets
#'
#' This function encloses a string with parentheses.
#'
#' @param x A character string to enclose.
#'
#' @return A new character string with `x` enclosed by parentheses.
#' @export
#' @keywords character
#' @examples
#' encloseBr("text") # returns "(text)"
encloseBr <- function(x) {
    enclose(x, "(", ")")
}

#' Get a Character at a Specific Index
#'
#' This function retrieves a character at a specific index from a string.
#'
#' @param x A character string.
#' @param index The index of the character to retrieve.
#' 
#' @return The character at the specified index.
#' @export
#' @keywords character
#' @examples
#' # Get the character at index 2
#' getChar("hello", 2)
getChar <- function(x, index) {
    substr(x, index, index)
}
