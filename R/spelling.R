#' Try to Split Words Based on Naming Convention
#'
#' This function attempts to split characters into its component words (and by default,
#' all in lowercase) based on  camelCase, PascalCase, or snake_case conventions. If
#' the string does not match any of these conventions, it returns all groups of letters.
#'
#' @param ... character(s) to be split, treated as a single vector after unlisting
#' @param conseq A logical indicating whether the `conseq` argument in [splitCamel()]/
#' [splitPascal()] should be `TRUE` or `FALSE`.
#' @param strictSnake A logical indicating the `strict` argument in [isSnakeCase()].
#' @param uncase A logical indicating whether to remove all casing in the output to
#' lowercase.
#' 
#' @return A list of character vectors, each containing the parts of the string
#'         split into individual words.
#' @export
#' @keywords spelling
#' @seealso \code{\link{splitCamel}}, \code{\link{splitPascal}}, \code{\link{splitSnake}},
#'          \code{\link{isCamelCase}}, \code{\link{isPascalCase}}, \code{\link{isSnakeCase}}
#'
#' @examples
#' trySplitWords("camelCaseExample")
#' trySplitWords("PascalCaseExample")
#' trySplitWords("snake_case_example", c("more_snake_cases"), "third_snake_case")
#' trySplitWords("some|random|case")
#' trySplitWords("Space Words", "UPPER_CASE", uncase = TRUE)
#'
trySplitWords <- function(
    ..., conseq = TRUE, strictSnake = FALSE, uncase = TRUE
) {

    x <- unlist(list(...), use.names = FALSE)
    assertthat::assert_that(is.character(x))

    lapply(x, function(y) {
        if (isCamelCase(y) || isPascalCase(y)) {
            out <- splitCamel(y, conseq = isTRUE(conseq))[[1]]
        } else if (isSnakeCase(y, strict = isTRUE(strictSnake))) {
            out <- splitSnake(y)[[1]]
        } else {
            out <- regmatches(y, gregexpr("[a-zA-Z]+", y))[[1]]
        }
        if (isTRUE(uncase)) return(tolower(out))
        out
    })
}

#' Split CamelCase or PascalCase Strings
#'
#' This function splits strings formatted in camelCase or PascalCase into their component
#' words. It can handle words where uppercase letters transition to lowercase letters, and it 
#' is capable of handling strings with sequences of uppercase letters followed by lowercase letters,
#' effectively separating acronyms from camelCase beginnings.
#'
#' @param x A character vector containing CamelCase or PascalCase strings to be split.
#' @param conseq Logical indicating whether consecutive uppercase letters should be
#'        treated as part of the previous word (TRUE) or as separate words (FALSE).
#'        Default is TRUE.
#'
#' @return A list of character vectors, each containing the parts of the corresponding
#'         CamelCase or PascalCase string split at the appropriate transitions.
#'         If `conseq` is FALSE, acronyms followed by words are separated.
#'
#' @examples
#' splitCamel("splitCamelCaseIntoWords")
#' splitCamel(c("fooBar", "FOOBar", "anotherFOOBarTest"), conseq = FALSE)
#'
#' @export
#' @keywords spelling
#' @source \url{https://stackoverflow.com/questions/8406974/splitting-camelcase-in-r}
splitCamel <- function(x, conseq = TRUE) {

    assertthat::assert_that(is.character(x))
    
    if (isTRUE(conseq)) {
        return(strsplit(
            x,
            "(?<=([A-Z])(?=[A-Z][a-z]))|(?<=[a-z])(?=[A-Z])",
            perl = TRUE
        ))
    }
    strsplit(gsub("([A-Z]{1})", " \\1", x), " ") %>%
        lapply(function(y) if (y[1] == "") y[-1] else y)
}

#' @rdname splitCamel
#' @export
splitPascal <- splitCamel

#' Split Snake Case String
#'
#' This function splits a string formatted in snake_case into its component words,
#' using underscores as delimiters. It is useful for parsing identifiers or variable
#' names that follow snake_case naming conventions.
#'
#' @param x A character string in snake_case to be split.
#'
#' @return A list of character vectors, each containing the parts of the string split at underscores.
#' @export
#' @keywords spelling
#' @examples
#' splitSnake("this_is_snake_case")
#' splitSnake("another_example_here")
#'
splitSnake <- function(x) {
    assertthat::assert_that(is.character(x))
    strsplit(x, "_", fixed = TRUE)
}

#' Check if String is camelCase
#'
#' This function checks if a given string adheres to camelCase naming conventions,
#' starting with a lowercase letter followed by any combination of upper and lower case letters.
#'
#' @param x A character string to check.
#'
#' @return TRUE if the string is camelCase, FALSE otherwise.
#' @keywords spelling
#' @export
#' @examples
#' isCamelCase("camelCase")   # returns TRUE
#' isCamelCase("CamelCase")   # returns FALSE
#' isCamelCase("camelcase")   # returns TRUE
#'
isCamelCase <- function(x) {
    assertthat::assert_that(is.character(x))
    grepl("^[a-z]+[A-Z]?([A-Za-z]*?)$", x)
}

#' Check if String is PascalCase
#'
#' This function checks if a given string adheres to PascalCase naming conventions,
#' starting with an uppercase letter followed by any combination of upper and lower case letters.
#'
#' @param x A character string to check.
#'
#' @return TRUE if the string is PascalCase, FALSE otherwise.
#' @keywords spelling
#' @export
#' @examples
#' isPascalCase("PascalCase") # returns TRUE
#' isPascalCase("pascalCase") # returns FALSE
#' isPascalCase("Pascalcase") # returns FALSE
isPascalCase <- function(x) {
    assertthat::assert_that(is.character(x))
    grepl("^[A-Z]+[a-z]?([A-Za-z]*?)$", x)
}

#' Check if String is snake_case
#'
#' This function checks if a given string adheres to snake_case naming conventions.
#' By default (strict = TRUE), it only allows lowercase letters separated by underscores.
#' If strict is FALSE, uppercase letters are also permitted.
#'
#' @param x A character string to check.
#' @param strict Logical indicating whether the string should strictly contain
#'        only lowercase letters (TRUE) or can include uppercase letters (FALSE).
#'        Default is TRUE.
#'
#' @return TRUE if the string is snake_case according to the specified strictness, FALSE otherwise.
#' @keywords spelling
#' @export
#' @examples
#' isSnakeCase("snake_case")        # returns TRUE
#' isSnakeCase("Snake_Case")        # returns FALSE
#' isSnakeCase("snake_case", FALSE) # returns TRUE
#' isSnakeCase("Snake_Case", FALSE) # returns TRUE
#' 
isSnakeCase <- function(x, strict = TRUE) {

    assertthat::assert_that(is.character(x))

    grepl(
        ifelse(
            isTRUE(strict),
            "^[a-z]+(_[a-z]+)*$",
            "^[A-Za-z]+(_[A-Za-z]+)*$"
        ),
        x
    )
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
    assertthat::assert_that(is.character(x))
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
    assertthat::assert_that(is.character(x))
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
    assertthat::assert_that(is.character(x))
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
    assertthat::assert_that(is.character(x))
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

    assertthat::assert_that(is.character(s))
    assertthat::assert_that(is.character(strset))
    assertthat::assert_that(is.function(distFunc) && (length(formals(distFunc)) >= 2))

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
