#' @keywords spelling
#' @export
getChar <- function(x, index) {
    substr(s, index, index)
}

#' @keywords spelling
#' @export
isVowel <- function(x) {
    tolower(x) %in% c("a", "e", "i", "o", "u")
}

#' @keywords spelling
#' @export
startsWithVowel <- function(x) {
    isVowel(getChar(x, 1))
}

#' @keywords spelling
#' @export
prependIndefArticle <- function(x) {
    paste("a", ifelse(startsWithVowel(x), "n", ""), " ", s, sep = "")
}

#' @keywords spelling
#' @export
stripSpaces <- function(x) {
    gsub(" ", "", x)
}

#' @keywords spelling
#' @export
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
