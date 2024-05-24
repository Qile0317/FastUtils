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
substrEnd <- function(x, start, endDiff) {
    substr(x, start, nchar(x) - endDiff)
}
