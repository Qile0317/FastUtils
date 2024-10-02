#' Join regex expressions by union
#'
#' This function simply joins a vector of regex characters by union,
#' and produces a single character regex in the form of `(foo)|(bar)`.
#'
#' @param ... character vectors of the regex expressions to join. Both
#' vectors and individual characters of any length will work
#'
#' @return a character of the unioned regex
#' @export
#' @keywords regex
#' @examples
#' joinRegex(c("^foo", "bar$"))
#' joinRegex("^foo", "bar$", "[bB]az")
#'
joinRegex <- function(...) {
    x <- unique(unlist(list(...)))
    if (length(x) == 1) return(x)
    encloseBr(paste(x, collapse = ")|("))
}

#' Remove Elements with Specified Name Regex
#'
#' This function removes elements from an indexable object (e.g., a named vector
#' or list)
#' where the names match a specified regular expression.
#'
#' @param x An indexable object (e.g., a named vector, list, or data frame).
#' @param pattern A character containing a regular expression(s) to match the
#' names of elements to be removed.
#' @param silent A logical indicating whether to silence a warning if no names
#' are detected.
#'
#' @return The input object with elements removed based on the name regex.
#' @export
#' @keywords regex
#'
#' @examples
#' myList <- list(a = 1, b_test = 2, c = 3, d_test = 4)
#' rmByName(myList, "_test")
#'
rmByName <- function(x, pattern, silent = FALSE) {

    assert_that(is.character(pattern))

    if (length(x) == 0) return(x)

    if (is.null(names(x))) {
        if (isFALSE(silent))
            warning("Input does not have named elements. No elements removed.")
        return(x)
    }

    x[!grepl(joinRegex(pattern), names(x))]
}

#' Search for a Pattern in Files within a Directory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The `greplDir` function searches for a specified pattern in all files within
#' a given directory. It allows for optional exclusion of files matching a
#' specified regular expression. Note that all files are assumed to be a single
#' string, with each line joined by the newline character `"\n"`.
#'
#' @param fpattern Character. The pattern to search for within the files.
#' @param dirPath Character. The path to the directory containing files to be
#' searched.
#' @param fIgnoreRegex Character. A regular expression to match file names that
#' should be ignored (default is NULL).
#' @param ... Additional arguments passed to [listFiles()], which are passed to
#' [list.files()]
#'
#' @return A named logical vector indicating which files contain the pattern.
#' The names attribute contains the file names.
#' @export
#' @keywords regex
#'
#' @examples
#' \donttest{
#' result <- tryCatch(
#'   greplDir("error", fIgnoreRegex = "\\.log$"),
#'   warning = function(w) c(exFname = TRUE),
#'   error = function(e) c(exFname = TRUE)
#' )
#' # its even more useful to use `base::which` on the result to
#' # get matches and mismatches - this returns it with names
#' # by default
#' which(result)
#' which(!result)
#' }
greplDir <- function(fpattern, dirPath = getwd(), fIgnoreRegex = NULL, ...) {

    allFiles <- listFiles(dirPath, ...)

    if (!is.null(fIgnoreRegex)) {
        allFiles <- Filter(
            function(x) !grepl(joinRegex(fIgnoreRegex), x),
            allFiles
        )
    }

    .greplFiles(allFiles, fpattern)
}

.greplFiles <- function(files, pattern, ignoreCase = FALSE) {

    pattern <- joinRegex(pattern)

    greplResults <- sapply(files, function(file) {
        tryCatch(
            {grepl(
                pattern = pattern,
                x = paste(readLines(file), collapse = "\n"),
                ignore.case = ignoreCase
            )},
            warning = function(w) {
                warning("pattern matching warning for file `", file, "`: ", w)
                FALSE
            },
            error = function(e) {
                warning("pattern matching error for file `", file, "`: ", e)
                FALSE
            }
        )

    })

    structure(greplResults, names = files)

}
