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
