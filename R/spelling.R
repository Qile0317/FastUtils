#' Get a Character at a Specific Index
#'
#' This function retrieves a character at a specific index from a string.
#'
#' @param x A character string.
#' @param index The index of the character to retrieve.
#' 
#' @return The character at the specified index.
#' @export
#' @keywords spelling
#' @examples
#' # Get the character at index 2
#' getChar("hello", 2)
getChar <- function(x, index) {
    substr(x, index, index)
}

#' Check if a Character is a Vowel
#'
#' This function checks if a character is a vowel.
#'
#' @param x A character.
#' 
#' @return TRUE if the character is a vowel, FALSE otherwise.
#' @export
#' @keywords spelling
#' @examples
#' # Check if 'a' is a vowel
#' isVowel("a")
#' # Check if 'b' is a vowel
#' isVowel("b")
isVowel <- function(x) {
    tolower(x) %in% c("a", "e", "i", "o", "u")
}

#' Check if a String Starts with a Vowel
#'
#' This function checks if a string starts with a vowel.
#'
#' @param x A character string.
#' 
#' @return TRUE if the string starts with a vowel, FALSE otherwise.
#' @export
#' @keywords spelling
#' @examples
#' # Check if "apple" starts with a vowel
#' startsWithVowel("apple")
#' # Check if "banana" starts with a vowel
#' startsWithVowel("banana")
startsWithVowel <- function(x) {
    isVowel(getChar(x, 1))
}

#' Prepend an Indefinite Article to a String
#'
#' This function prepends an indefinite article ("a" or "an") to a string based on whether it starts with a vowel or not.
#'
#' @param x A character string.
#' 
#' @return The string with an indefinite article prepended.
#' @export
#' @keywords spelling
#' @examples
#' # Prepend an indefinite article to "apple"
#' prependIndefArticle("apple")
#' # Prepend an indefinite article to "banana"
#' prependIndefArticle("banana")
prependIndefArticle <- function(x) {
    paste("a", ifelse(startsWithVowel(x), "n", ""), " ", x, sep = "")
}

#' Remove Spaces from a String
#'
#' This function removes spaces from a character string.
#'
#' @param x A character string.
#' 
#' @return The string with spaces removed.
#' @export
#' @keywords spelling
#' @examples
#' # Remove spaces from "hello world"
#' stripSpaces("hello world")
stripSpaces <- function(x) {
    gsub(" ", "", x)
}

#' Find the Closest Word in a Set to a Given Word
#'
#' This function finds the closest word in a set of words to a given word based on a specified distance function.
#'
#' @param s A character string.
#' @param strset A set of character strings.
#' @param distFunc A function to compute distance between strings. Default is `utils::adist`.
#' 
#' @return The closest word in the set to the given word.
#' @export
#' @keywords spelling
#' @examples
#' # Find the closest word to "hello" in the set c("hallo", "hullo", "hey")
#' closestWord("hello", c("hallo", "hullo", "hey"))
closestWord <- function(s, strset, distFunc = utils::adist) {

    strset <- unique(strset)
    if (length(strset) == 1) return(strset)

    strset_lowercase <- tolower(strset)
    s <- tolower(s)

    closest_w <- strset_lowercase[1]
    closest_dist <- distFunc(s, closest_w)

    for(i in 2:length(strset_lowercase)) {
        curr_dist <- distFunc(s, strset_lowercase[i])
        if (curr_dist < closest_dist) {
            closest_w <- strset[i]
            closest_dist <- curr_dist
        }
    }
    closest_w
}
